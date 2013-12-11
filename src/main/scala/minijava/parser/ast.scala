package minijava {

package parser {
	import scala.reflect.runtime.universe._

	sealed trait Term {
		def children(): List[Term] = Nil
	}

	sealed abstract class Statement extends Term
	sealed abstract class Exp extends Term

	sealed abstract class BinOp
	case object Plus extends BinOp
	case object Minus extends BinOp
	case object Mul extends BinOp
	case object LAnd extends BinOp
	case object Lt extends BinOp
	case object Subscript extends BinOp

	sealed abstract class UnOp
	case object Neg extends UnOp
	case object Len extends UnOp

	sealed abstract class Type
	case object TBool extends Type
	case object TInt extends Type
	case object TIntArray extends Type
	case object Void extends Type
	case class TClass(id: String) extends Type

	// expressions
	case object True extends Exp
	case object False extends Exp
	case class BinExp( op: BinOp, left: Exp, right: Exp ) extends Exp {
		override def children() = List( left, right )
	}
	case class UnExp( op: UnOp, right: Exp ) extends Exp {
		override def children() = List( right )
	}

	// toplevel
	case class Program(
		main: MainClass,
		classes: List[ClassDecl]
	) extends Term {
		override def children() = main :: classes
	}

	// statements
	case class IfStmt(
		cond: Exp,
		then_body: Statement,
		else_body: Statement
		) extends Statement {
		override def children() = List( cond, then_body, else_body )
	}

	case class WhileStmt(
		cond: Exp,
		body: Statement
		) extends Statement {
		override def children() = List( cond, body )
	}

	case class BlockStmt(
		body: List[Statement]
		) extends Statement {
		override def children() = body
	}

	case class PrintStmt(
		exp: Exp
		) extends Statement {
		override def children() = List( exp )
	}

	case class AssignStmt(
		id: String,
		value: Exp
		) extends Statement {
		override def children() = List( value )
	}

	case class ArrayAssignStmt(
		id: String,
		index: Exp,
		value: Exp
		) extends Statement {
		override def children() = List( index, value )
	}

	// class definitions
	case class MainClass(
		id: String,
		argv: String,
		body: Statement
		) extends Term {
		override def children() = List( body )
	}

	case class ClassDecl(
		id: String,
		parent: Option[String],
		fields: List[Field],
		methods: List[MethodDecl]
		) extends Term {
		override def children() = fields ::: methods
	}

	case class VarDecl(
		vtype: Type,
		id: String
		) extends Term

	case class Field(
		vtype: Type,
		id: String
	) extends Term

	case class Param(
		vtype: Type,
		id: String
		) extends Term

	case class MethodDecl(
		mtype: Type,
		id: String,
		params: List[Param],
		vars: List[VarDecl],
		body: List[Statement],
		ret_exp: Exp
		) extends Term {

		override def children() = params ::: vars ::: ( body :+ ret_exp )
	}

	// statements
	case class Print(
		exp: Exp
		) extends Statement {

		override def children() = List( exp )
	}

	case class Assign(
		vid: String,
		exp: Exp
		) extends Statement {

		override def children() = List( exp )
	}

	// expressions
	case class IntValue( value: Int ) extends Exp

	} // end of parser package
} // end of minijava package
