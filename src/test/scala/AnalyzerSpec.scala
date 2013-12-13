import minijava.analysis._
import minijava.parser.{Parser, Program}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.Matchers

trait AnalyzerSpec extends Matchers {

	/**
	 * Return a common header for all your tests
	 */
	val header: String = ""

	protected def setup(input: String): (Program, SymbolTable) = {
		Parser(header + input) match {
			case Some(p: Program) => (p, SymbolTable.build(p))
			case _ => throw new TestFailedException("Sorry, test input won't parse as program", 1)
		}
	}

	protected def exec(input: String): AnalysisResult

	def negative(input: String, substr: String = "") = {
		val result = exec(input)
		result shouldBe a[ Failure ]
		result.asInstanceOf[ Failure ].msg.toLowerCase should include(substr.toLowerCase)
	}

	def positive(input: String) = {
		exec(input) should equal(Success)
	}
}
