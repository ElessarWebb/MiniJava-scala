package minijava.parser

sealed abstract class Term
sealed abstract class Statement extends Term
sealed abstract class Exp extends Term
sealed abstract class Type extends Term

sealed abstract class BinOp extends Term
case object Plus extends BinOp
case object Minus extends BinOp
case object Mul extends BinOp
case object LAnd extends BinOp
case object Lt extends BinOp
case object Subscript extends BinOp

sealed abstract class UnOp extends Term
case object Neg extends UnOp
case object Len extends UnOp

case object TBool extends Type
case object TInt extends Type
case object TIntArray extends Type
case class TClass(id: String) extends Type

// more literals
case object True extends Exp
case object False extends Exp
case class BinExp( op: BinOp, left: Exp, right: Exp ) extends Exp
case class UnExp( op: UnOp, right: Exp ) extends Exp

// statements
case class IfStmt(
	cond: Exp,
	then_body: Statement,
	else_body: Statement
	) extends Statement

case class BlockStmt(
	body: List[Statement]
	) extends Statement

// class definitions
case class MainClass(
	id: String,
	argv: String,
	body: Statement
	) extends Term

case class ClassDecl(
	id: String,
	parent: Option[String],
	fields: List[VarDecl],
	methods: List[MethodDecl]
	) extends Term

case class VarDecl(
	vtype: Type,
	id: String
	) extends Term

// alias for fields
type Field = VarDecl

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
	) extends Term

// statements
case class Print(
	exp: Exp
	) extends Statement

case class Assign(
	vid: String,
	exp: Exp
	)

// expressions
case class IntValue( value: Int ) extends Exp
