import minijava.analysis.Failure
import minijava.parser.Program
import minijava.parser.{IntValue, Program, Parser}
import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import minijava.analysis._
import scala.Some

trait AnalyzerSpec extends Matchers {

	/**
	 * Return a common header for all your tests
	 */
	val header: String = ""

	protected def setup( input: String ): ( Program, SymbolTable )  = {
		Parser( header + input ) match {
			case Some(p:Program) => ( p, SymbolTable.build( p ))
			case _ => throw new TestFailedException( "Sorry, test input won't parse as program", 1 )
		}
	}
}

class NameAnalyzerSpec extends FlatSpec with AnalyzerSpec {

	override val header =
		""" class Main {
		  |		public static void main( String[] argv ) {
		  |  		System.out.println(3);
		  | 	}
		  | }
		  |
		""".stripMargin

	behavior of "The name analyzer"

	def negative( input: String, substr: String = "") = {
		val result = SemanticAnalysis.apply _ tupled setup( input )
		result shouldBe a [Failure]
		result.asInstanceOf[ Failure ].msg.toLowerCase should include ( substr.toLowerCase )
	}

	def positive( input: String ) = {
		(SemanticAnalysis.apply _ tupled setup( input )) should equal (Success)
	}

	it should "fail on undefined parents" in {
		negative( "class B extends A {}", "missing definition" )
	}

	it should "not fail on classes with later defined parents" in {
		positive(
			""" class B extends A {}
			  | class A {}
			""".stripMargin )
	}

	it should "fail on declaring fields of unknown types" in {
		negative(
			"""
			  | class A {
			  | 	B b;
			  | }
			""".stripMargin, "missing definition" )
	}

	it should "not fail on declaring fields of known types" in {
		positive(
			"""
			  | class A {
			  | 	B b;
			  | }
			  | class B {}
			""".stripMargin)
	}

	it should "fail on declaring vars of unknown types" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		B b;
			  |  		return 1;
			  |  	}
			  | }
			""".stripMargin, "missing definition" )
	}

	it should "fail on declaring params of unknown types" in {
		negative(
			"""
			  | class A {
			  | 	public int a( B b ) {
			  |  		return 1;
			  |  	}
			  | }
			""".stripMargin, "missing definition" )
	}

	it should "fail on using undefined variable (shadowing method name)" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		return a;
			  |  	}
			  | }
			""".stripMargin, "undeclared var" )
	}

	it should "not fail on using parameter" in {
		positive(
			"""
			  | class A {
			  | 	public int a( int a ) {
			  |  		return a;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "not fail on using defined variable" in {
		positive(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int a;
			  |  		return a;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "not fail on using defined field" in {
		positive(
			"""
			  | class A {
			  | 	int f;
			  | 	public int a() {
			  |  		return f;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "not fail on using undefined field" in {
		negative(
			"""
			  | class B {
			  | 	int f;
			  | }
			  | class A {
			  | 	public int a() {
			  |  		return f;
			  |  	}
			  | }
			""".stripMargin, "undeclared var")
	}

	it should "not fail on using defined variables in print statement" in {
		positive(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |  		System.out.println( u );
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "not fail on using defined variables in if statement" in {
		positive(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		if( u < t ) {} else {}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "fail on using undefined variables in print statement" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |    		System.out.println( t );
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin, "undeclared var" )
	}

	it should "not fail on using defined variables in nested statement" in {
		positive(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		if( u < t ) {
			  |      		if ( t < u ) {} else {}
			  |      	} else {}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin)
	}

	it should "fail on using undefined variables in nested if statement" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		if( u < t ) {
			  |      		if ( x < u ) {} else {}
			  |      	} else {}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin, "undeclared var" )
	}

	it should "not fail on using defined variables in while statement" in {
		positive(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		while( u < t ) {
			  |      		System.out.println( t );
			  |      	}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin )
	}

	it should "fail on using undefined variables in while statement body" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		while( u < t ) {
			  |      		System.out.println( x );
			  |      	}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin, "undeclared var" )
	}

	it should "fail on using undefined variables in while statement condition" in {
		negative(
			"""
			  | class A {
			  | 	public int a() {
			  |  		int u;
			  |  		int t;
			  |    		while( u < x ) {}
			  |    		return 1;
			  |  	}
			  | }
			""".stripMargin, "undeclared var" )
	}
}
