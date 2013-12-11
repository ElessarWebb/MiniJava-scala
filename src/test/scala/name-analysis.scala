import minijava.parser.{IntValue, Program, Parser}
import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import minijava.analysis.{Failure, Success, NameAnalysis, SymbolTable}

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
		val result = NameAnalysis.apply _ tupled setup( input )
		result shouldBe a [Failure]
		result.asInstanceOf[ Failure ].msg.toLowerCase should include ( substr.toLowerCase )
	}

	def positive( input: String ) = {
		(NameAnalysis.apply _ tupled setup( input )) should equal (Success)
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
}
