import org.scalatest._
import minijava.analysis._

class TypeAnalyzerSpec extends FlatSpec with AnalyzerSpec {

	override val header =
		""" class Main {
		  |		public static void main( String[] argv ) {
		  |  		System.out.println(3);
		  | 	}
		  | }
		  |
		""".stripMargin

	behavior of "The type analyzer"

	override protected def exec( input: String ): AnalysisResult = {
		val inp = setup( input )
		SemanticAnalyzer.analyze(inp._1, inp._2)
	}

	it should "Fail on boolean fields getting integers assigned" in {
		negative(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = true;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin, "expected expression" )
	}

	it should "not fail on boolean fields getting booleans assigned" in {
		positive(
			"""
			  | class A {
			  | 	boolean a;
			  |  	public int b() {
			  |   		a = true;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}
}
