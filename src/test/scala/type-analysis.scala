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

	it should "correctly deduce type of lt expression" in {
		positive(
			"""
			  | class A {
			  | 	boolean a;
			  |  	public int b() {
			  |   		a = 1 < 2;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of sum expression" in {
		negative(
			"""
			  | class A {
			  | 	boolean a;
			  |  	public int b() {
			  |   		a = 1 + 2;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of mult expression" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 * 2;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of complex int expression" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 * ( 3 + 4 * 5 ) - 6;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of subscript expression" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		int[] c;
			  |   		a = c[0];
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "fail on bool return type where int is declared" in {
		negative(
			"""
			  | class A {
			  |  	public int b() {
			  |    		return true;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "fail on int return type where int[] is declared" in {
		negative(
			"""
			  | class A {
			  |  	public int[] b() {
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of call to method on self" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = this.b();
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of call to method on other instance of self" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		A inst;
			  |     	inst = new A();
			  |   		a = inst.b();
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "correctly deduce type of call to method on instance of other class" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		B inst;
			  |     	inst = new B();
			  |   		a = inst.c();
			  |    		return 1;
			  |     }
			  | }
			  | class B {
			  | 	public int c() {
			  |  		return 1;
			  |   	}
			  | }
			""".stripMargin)
	}

	it should "fail on assignment with call to method of wrong type" in {
		negative(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		B inst;
			  |     	inst = new B();
			  |   		a = inst.c();
			  |    		return 1;
			  |     }
			  | }
			  | class B {
			  | 	public boolean c() {
			  |  		return true;
			  |   	}
			  | }
			""".stripMargin, "expected expression" )
	}

	it should "fail on bool assigned to int array element" in {
		negative(
			"""
			  | class A {
			  | 	int[] a;
			  |  	public int b() {
			  |   		a[0] = true;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "succeed on int assigned to int array element" in {
		positive(
			"""
			  | class A {
			  | 	int[] a;
			  |  	public int b() {
			  |   		a[0] = 1;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "fail on boolean use as index in array assignment" in {
		negative(
			"""
			  | class A {
			  | 	int[] a;
			  |  	public int b() {
			  |   		a[true] = 1;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin, "index" )
	}

	it should "succeed on int expr used as index in array assignment" in {
		positive(
			"""
			  | class A {
			  | 	int[] a;
			  |  	public int b() {
			  |   		a[1+4*8] = 1;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "fail on boolean use as left operand of sum" in {
		negative(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 + true;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin, "right operand" )
	}

	it should "fail on boolean use as left operand of subtraction" in {
		negative(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 - true;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin, "right operand" )
	}

	it should "fail on boolean use as left operand of subscript" in {
		negative(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = true[1];
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin, "left operand" )
	}

	it should "succeed on intarray use as left operand of subscript" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		int[] b;
			  |   		a = b[1];
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "succeed on ints as operand of addition" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 + a;
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}

	it should "succeed on ints as operand of subtraction" in {
		positive(
			"""
			  | class A {
			  | 	int a;
			  |  	public int b() {
			  |   		a = 1 - this.b();
			  |    		return 1;
			  |     }
			  | }
			""".stripMargin)
	}
}
