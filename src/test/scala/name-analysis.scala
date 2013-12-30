import org.scalatest._
import minijava.analysis._

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

	override protected def exec( input: String ): AnalysisResult[Boolean] = {
		val inp = setup( input )
		inp._1.foldDown[AnalysisResult[Boolean]](Success(true)) {
			// perform name analysis on this node
			// and combine the results
			(r, t) => r & NameAnalyzer.analyze(t)(inp._2)
		}
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
