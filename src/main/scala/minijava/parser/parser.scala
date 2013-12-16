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
	def new_kw = "new"

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
	def apply( input: String ): ParseResult[Exp] = super.parseAll( exp, input )
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

	// atoms
	def simple_term: Parser[Exp] = intval | boolval | ref | self

	// level 0 precedence
	def term_0: Parser[Exp] = ( "(" ~> exp <~ ")" ) | simple_term

	// level 1 precedence
	def call: Parser[Exp] = ( term_0 <~ "." ) ~ id ~ ( "(" ~> repsep( exp, "," ) <~ ")" ) ^^ {
		case ( obj ) ~ id ~ ( args ) => Call( obj, id, args )
	}
	def subscript: Parser[Exp] = term_0 ~ ( "[" ~> exp <~ "]" ) ^^ {
		case arr ~ ( index ) => BinExp( Subscript, arr, index )
	}
	def term_1: Parser[Exp] = call | subscript | term_0

	def create_array: Parser[Exp] = new_kw ~> "int" ~> "[" ~> exp <~ "]" ^^ { case exp => NewArray(exp) }
	def create_obj: Parser[Exp] = new_kw ~> id <~ ( "(" ~ ")" ) ^^ { case id => NewObject( id ) }
	def not_term: Parser[Exp] = op_not ~> term_1 ^^ { x => UnExp( Neg, x ) }

	// level 2 precedence
	def term_2: Parser[Exp] = create_array | create_obj | not_term | term_1

	// level 3 precedence
	def exp_mul = term_2 ~ rep( op_mul ~> term_2 ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) => BinExp( Mul, acc, r )}
	}

	// level 4 precedence
	def exp_sum: Parser[Exp] = exp_mul ~ rep(( op_plus | op_minus ) ~ exp_mul ) ^^ {
		case left ~ rights => rights.foldLeft( left ) { ( acc, r ) =>
			r match {
				case op ~ right => BinExp( op, acc, right )
			}
		}
	}

	// level 5 precedence
	def exp_lt: Parser[Exp] = exp_sum ~ ( op_lt ~> exp_sum ).? ^^ {
		case left ~ Some(right) => BinExp( Lt, left, right )
		case left ~ None => left
	}

	// level 6 precedence
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
	def apply( input: String ): ParseResult[Program] = {
		parseAll( program, input )
	}
}
