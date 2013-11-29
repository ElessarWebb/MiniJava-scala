package minijava.parser

import scala.util.parsing.combinator._

/**
 * Op precedence (high to low): !, *, (+,-), <, &&
 */
trait ExpressionParsers extends MiniJavaLiteralParsers {
	def intval = int ^^ { x => IntValue( x ) }
	def boolval = ( true_kw | false_kw ) ^^ {
		case "true" => True
		case "false" => False
	}

	def op_not = "!" ^^ { _ => Neg }
	def op_and = "&&" ^^ { _ => LAnd }
	def op_plus = "+" ^^ { _ => Plus }
	def op_minus = "-" ^^ { _ => Minus }
	def op_mul = "*" ^^ { _ => Mul }
	def op_lt = "<" ^^ { _ => Lt }

	def simpleterm: Parser[Exp] = intval | boolval
	def not_term: Parser[Exp] = op_not ~> simpleterm ^^ { x => UnExp( Neg, x ) }
	def term: Parser[Exp] = simpleterm | not_term | ( "(" ~> exp <~ ")" )

	// convert a series of log. ands to a nested and expression
	def exp_mul = term ~ rep( op_mul ~> term ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) => BinExp( Mul, acc, r )}
	}

	// convert a series of additions/subtractions to the appropriate binary expressions
	def exp_sum: Parser[Exp] = exp_mul ~ rep(( op_plus | op_minus ) ~ exp_mul ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) =>
			r match {
				case op ~ right => BinExp( op, acc, right )
			}
		}
	}

	// a < b < c is not supported
	// convert a < b to BinExp
	def exp_lt: Parser[Exp] = exp_sum ~ ( op_lt ~> exp_sum ).? ^^ {
		case left ~ Some(right) => BinExp( Lt, left, right )
		case left ~ None => left
	}

	// convert a && b && ...
	def exp_land = exp_lt ~ rep( op_and ~> exp_lt ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) => BinExp( LAnd, acc, r )}
	}

	def exp:Parser[Exp] = exp_land
}

object ExpressionParser extends parser.ExpressionParsers {
	def apply( input: String ) = parseAll( exp, input )
}

trait MiniJavaParsers extends MiniJavaLiteralParsers with ExpressionParsers {

	// goal
	def program = mainclass ~ rep(classdecl)

	// main class definition
	def typ: Parser[Type] = {
		"int" ^^ { _ => TInt } |
			"boolean" ^^ { _ => TBool } |
			"int" ~ "[" ~ "]" ^^ { _ => TIntArray } |
			id ^^ { x => TClass(x) }
	}

	def decl = typ ~ id

	def field: Parser[Field] = decl <~ ";" ^^ {
		case t ~ id => VarDecl( t, id )
	}

	def var_decl = decl <~ ";" ^^ { case t ~ i => VarDecl( t, i ) }

	def statement = id <~ ";" ^^ { _ => BlockStmt( List[Statement]() ) }
	def param: Parser[Param] = decl ^^ {
		case t ~ id => Param( t, id )
	}

	def method = {
		public_kw ~> typ ~ id ~ ( "(" ~> rep( param ) <~ ")" ) ~
			( "{" ~> rep( var_decl ) ~ rep( statement ) ~ ( return_kw ~> exp <~ ";" ) <~ "}" ) ^^ {
			case ( t ~ i ) ~ ps ~ ( ds ~ ss ~ r ) => MethodDecl( t, i, ps, ds, ss, r )
		}
	}

	def classdecl = {
		class_kw ~> id ~ ( extends_kw ~> id ).? ~
			( "{" ~> field.* ~ method.* <~ "}" ) ^^ {
			case id ~ p ~ ( fs ~ ms ) => ClassDecl( id, p, fs, ms )
		}
	}

	private def mainmethod: Parser[(String,Statement)] =  {
		( public_kw ~ "static" ~ "void" ~ "main" ~ "(" ~ "String" ~ "[" ~ "]"  ) ~> id ~
			( ")" ~> "{" ~> statement <~  "}" ) ^^ {
			case id ~ ss => ( id, ss )
		}
	}

	def mainclass = {
		( "class" ~> id ) ~ ( "{" ~> mainmethod <~  "}" ) ^^ {
			case c ~ main => MainClass( c, main._1, main._2 )
		}
	}
}
object MiniJavaParser extends MiniJavaParsers {
	def apply( input: String ) = parseAll( program, input )
}

trait MiniJavaLiteralParsers extends RegexParsers {
	// literals
	def int: Parser[Int] = "[-]?[0-9]+".r ^^ { _.toInt }
	def id: Parser[String] = not( reserved ) ~> "[a-zA-Z][a-zA-Z0-9_]*".r

	def class_kw: Parser[String] = "class"
	def extends_kw: Parser[String] = "extends"
	def public_kw: Parser[String] = "public"
	def return_kw: Parser[String] = "return"
	def while_kw: Parser[String] = "while"
	def if_kw: Parser[String] = "if"
	def else_kw: Parser[String] = "else"
	def true_kw: Parser[String] = "true"
	def false_kw: Parser[String] = "false"

	def reserved = class_kw | extends_kw | public_kw | return_kw | while_kw | if_kw | else_kw | true_kw | false_kw
}
