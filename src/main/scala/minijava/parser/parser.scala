package minijava.parser

import scala.util.parsing.combinator._

/**
 * Defines parsers for all minijava terminals
 */
trait LiteralParsers extends RegexParsers {
	// literals
	def int: Parser[Int] = "[-]?[0-9]+".r ^^ { _.toInt }
	def id: Parser[String] = not( reserved ) ~> "[a-zA-Z][a-zA-Z0-9_]*".r

	// some keywords
	def class_kw: Parser[String] = "class"
	def extends_kw: Parser[String] = "extends"
	def public_kw: Parser[String] = "public"
	def return_kw: Parser[String] = "return"
	def while_kw: Parser[String] = "while"
	def if_kw: Parser[String] = "if"
	def else_kw: Parser[String] = "else"
	def true_kw: Parser[String] = "true"
	def false_kw: Parser[String] = "false"
	def println_kw = "System" ~ "." ~ "out" ~ "." ~ "println"
	def this_kw = "this"

	//
	def reserved = {
		class_kw |
		extends_kw |
		public_kw |
		return_kw |
		while_kw |
		if_kw |
		else_kw |
		true_kw |
		false_kw |
		this_kw
	}
}

object ExpressionParser extends ExpParsers {
	def apply( input: String ): Option[Exp] = super.parseAll( exp, input ) match {
		case Success( exp, _ ) => Some(exp)
		case _ => None
	}
}

/**
 * Parses a minijava expression
 * Operator precedence (high to low): (call), !, *, (+,-), <, &&
 */
trait ExpParsers extends LiteralParsers {
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
	def ref = id ^^ { Ref }
	def self = this_kw ^^ { _ => This }

	def simpleterm: Parser[Exp] = intval | boolval | ref | self

	def term: Parser[Exp] = ( "(" ~> exp <~ ")" ) | simpleterm

	def call: Parser[Exp] = ( term <~ "." ) ~ id ~ ( "(" ~> repsep( exp, "," ) <~ ")" ) ^^ {
		case ( obj ) ~ id ~ ( args ) => Call( obj, id, args )
	} | term

	def not_term: Parser[Exp] = op_not ~> call ^^ { x => UnExp( Neg, x ) } | call

	def exp_mul = not_term ~ rep( op_mul ~> not_term ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) => BinExp( Mul, acc, r )}
	}

	def exp_sum: Parser[Exp] = exp_mul ~ rep(( op_plus | op_minus ) ~ exp_mul ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) =>
			r match {
				case op ~ right => BinExp( op, acc, right )
			}
		}
	}

	// a < b < c is not supported
	def exp_lt: Parser[Exp] = exp_sum ~ ( op_lt ~> exp_sum ).? ^^ {
		case left ~ Some(right) => BinExp( Lt, left, right )
		case left ~ None => left
	}

	def exp_land = exp_lt ~ rep( op_and ~> exp_lt ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) => BinExp( LAnd, acc, r )}
	}

	def exp:Parser[Exp] = exp_land
}

trait Parsers extends LiteralParsers with ExpParsers {

	// goal
	def program = mainclass ~ rep(classdecl) ^^ { case main ~ classes => Program( main, classes ) }

	def typ: Parser[Type] = {
			"int" ~ "[" ~ "]" ^^ { _ => TIntArray } |
			"int" ^^ { _ => TInt } |
			"boolean" ^^ { _ => TBool } |
			id ^^ { x => TClass(x) }
	}

	/**
	 * Parse generic declaration
	 * used for fields/vardefs/params
	 */
	private def decl = typ ~ id

	def field: Parser[Field] = decl <~ ";" ^^ {
		case t ~ id => Field( t, id )
	}

	def vardecl = decl <~ ";" ^^ { case t ~ i => VarDecl( t, i ) }

	def param: Parser[Param] = decl ^^ {
		case t ~ id => Param( t, id )
	}

	def method: Parser[MethodDecl] = {
		public_kw ~> typ ~ id ~ ( "(" ~> rep( param ) <~ ")" ) ~
			( "{" ~> rep( vardecl ) ~ rep( statement ) ~ ( return_kw ~> exp <~ ";" ) <~ "}" ) ^^ {
			case ( t ~ i ) ~ ps ~ ( ds ~ ss ~ r ) => MethodDecl( t, i, ps, ds, ss, r )
		}
	}

	def classdecl: Parser[ClassDecl] = {
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

	def mainclass: Parser[MainClass] = {
		( "class" ~> id ) ~ ( "{" ~> mainmethod <~  "}" ) ^^ {
			case c ~ main => MainClass( c, main._1, main._2 )
		}
	}

	def statement:Parser[Statement] = {
		block_stmt |
		if_stmt |
		while_stmt |
		print_stmt |
		assign_stmt |
		array_assign_stmt
	}

	def block_stmt = "{" ~> rep( statement ) <~ "}" ^^ { case x => BlockStmt( x ) }

	def if_stmt = "if" ~> ( "(" ~> exp <~ ")" ) ~ statement ~ ( "else" ~> statement ) ^^ {
		case ( exp ) ~ thenst ~ ( elsest ) => IfStmt( exp, thenst, elsest )
	}

	def while_stmt = "while" ~> ( "(" ~> exp <~ ")" ) ~ statement ^^ {
		case ( exp ) ~ stmts => WhileStmt( exp, stmts )
	}

	def print_stmt = println_kw ~ "(" ~> exp <~ ")" <~ ";" ^^ { case e => PrintStmt( e ) }

	def assign_stmt = id ~ ( "=" ~> exp ) <~ ";" ^^ { case id ~ ( value ) =>
		AssignStmt( id, value )
	}

	def array_assign_stmt = id ~ ( "[" ~> exp <~ "]" ) ~ ( "=" ~> exp ) <~ ";" ^^ { case id ~ ( index ) ~ ( value ) =>
		ArrayAssignStmt( id, index, value )
	}
}

object Parser extends Parsers {
	def apply( input: String ): Option[Program] = {
		parseAll( program, input ) match {
			case Success( program, _ ) => Some(program)
			case _ => None
		}
	}
}
