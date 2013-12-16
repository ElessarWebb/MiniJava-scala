import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import minijava.parser._

trait ParserSpec extends Matchers {

	class ParseException extends Exception

	protected def parse( s:String ): Term

	/**
	 * Partial function that matches every term and returns false
	 */
	val else_false: PartialFunction[Term,Boolean] = { case _:Term => false }

	/**
	 * Helper function for pattern matching AST's
	 *
	 * Provides some syntactic sugar by accepting a partial function { case SomeTerm => true } as a snd argument
	 * and combining that with a match all Terms PartialFunction before pattern matching the parsed first argument
	 * against it. This way we ensure that AST is matched against exhaustive patterns
	 */
	protected def positive[U]( s:String )( f: PartialFunction[Term,Boolean] = else_false ): Unit = {
		if( !(f orElse else_false)(parse( s )) )
			throw new TestFailedException( "Woops, didn't expect that AST", 1 )
	}

	/**
	 * A negative test case helper for parsers
	 */
	protected def negative( input:String ) = intercept[ParseException] {
		parse( input )
	}
}

class ExpParsersSpec extends FlatSpec with ParserSpec with Matchers {

	protected def parse( s:String ): Exp = {
		ExpressionParser( s ) match {
			case ExpressionParser.Success( exp:Exp, _ ) => exp
			case _ => throw new ParseException()
		}
	}

	behavior of "The  Expression Parser"

	it should "parse valid integers" in {
		parse( "3" ) should equal( IntValue(3) )
		parse( "-3" ) should equal( IntValue(-3) )
		parse( "3" ) should equal( IntValue(+3) )
	}

	it should "parse sums" in {
		parse( "3 + 4" ) should equal( BinExp( Plus, IntValue(3), IntValue(4) ))
		parse( "3 + -4" ) should equal( BinExp( Plus, IntValue(3), IntValue(-4) ))
		parse( "-3 + 4" ) should equal( BinExp( Plus, IntValue(-3), IntValue(4) ))
	}

	it should "precede mul over sum" in {
		positive( "3 + 4 * 5" ) { case BinExp( Plus, _, BinExp( Mul, _, _ )) => true }
		positive( "3 * 4 + 5" ) { case BinExp( Plus, BinExp( Mul, _, _ ), _ ) => true }
	}

	it should "precede negation over land" in {
		positive( "true && !false" ) { case BinExp( LAnd, _, UnExp( Neg, _ )) => true }
		positive( "!true && false" ) { case BinExp( LAnd, UnExp( Neg, _ ), _ ) => true }
	}

	it should "precede braced subexpressions" in {
		positive( "( 3 + 4 ) * 5" ) { case BinExp( Mul, _, _ ) => true }
	}

	it should "parse calls without params" in {
		positive( "this.something()" ) { case Call( _, "something", Nil ) => true }
	}

	it should "parse calls with a single param" in {
		positive( "this.something( 3 )" ) { case Call( _, "something", List( IntValue( 3 ))) => true }
	}

	it should "parse calls with multiple param" in {
		positive( "this.something( 3, 4, 5 )" ) { case Call( _, "something", List( _, _, _ )) => true }
	}

	it should "parse calls with multiple complex param" in {
		positive( "this.something( 3, 4 * 5 + !true, 5 )" ) { case Call( _, "something", List( _, _, _ )) => true }
	}

	it should "parse nested calls" in {
		positive( "this.something( 3, that.another(), 5 )" ) { case Call(
			_,
			"something",
			List( _, Call( _, "another", _ ), _ )
		) => true }
	}

	it should "parse object creations" in {
		positive( "new A()" ) { case NewObject( "A" ) => true }
	}

	it should "not parse object creation with arguments" in {
		negative( "new A( 3 )" )
	}

	it should "parse array creation" in {
		positive( "new int[ 3 ]" ) { case NewArray( IntValue( 3 )) => true }
	}

	it should "not parse array creations without index exp" in {
		negative( "new int[]" )
	}

	it should "parse array creation with complex expressions" in {
		positive( "new int[ 3 + 8 * 4 * a.b() ]" ) { case NewArray( _ ) => true }
	}
}

class MiniJavaParserSpec extends FlatSpec with ParserSpec with Matchers {

	protected def parse( s:String ): Program = {
		Parser( s ) match {
			case Parser.Success(program:Program, _) => program
			case _ => throw new ParseException()
		}
	}

	behavior of "The MiniJava Parser"

	it should "parse a main class as a program" in {
		positive(
			""" class Main {
			  |		public static void main( String[] argv ) {
			  |  		System.out.println(3);
			  | 	}
			  | }
			""".stripMargin) { case Program( MainClass( "Main", "argv", _ ), Nil ) => true }
	}

	it should "not parse an empty class as a program" in {
		 negative {
			""" class Main {
			  | }
			""".stripMargin
		}
	}
}