package minijava {

import minijava.parser._
import minijava.utils.CompileException

package object analysis {

	type TypeResult = Either[Failure,Type]

	/**
	 * Packs some handy typetask related conversions
	 */
	object TypeResult {
		
		class WrappedTypeResult(t: TypeResult) {

			/**
			 * Conditionally evalutes anything that would evaluate to an analysisresult
			 * given that the typeresult is succesful-ish
			 */
			def <::(f: => AnalysisResult): AnalysisResult = t match {
				case Left(fail) => fail
				case Right(_) => f
			}

			def +(right: TypeResult): TypeResult = t match {
				case l@Left(_) => l
				case r@Right(_) => right
			}
		}

		implicit def type_to_typetask(t: Type): TypeResult = Right(t)
		implicit def failure_to_typetask(f: Failure): TypeResult = Left(f)
		implicit def typetask_to_result(e: Either[Failure,Type]): AnalysisResult = e match {
			case Left(f) => f
			case Right(t) => Success
		}

		implicit def task_to_wrappedtask(t:TypeResult): WrappedTypeResult = new WrappedTypeResult(t)
	}
}

package analysis {

import com.sun.org.apache.xalan.internal.xsltc.compiler.CompilerException

abstract sealed class AnalysisResult {
	def and(right: AnalysisResult): AnalysisResult
}

case object Success extends AnalysisResult {

	def and(right: AnalysisResult) = right match {
		case Success => Success
		case r: Failure => r
	}

}

case class Failure(msg: String, next: Option[ Failure ] = None) extends AnalysisResult {

	implicit def failToOpt(f: Failure) = Some(f)

	def and(right: AnalysisResult): Failure = right match {
		case f: Failure => Failure(msg, f)
		case Success => this
	}

	override def toString: String = {
		val s = s"$msg"
		val more = len() - 1

		if( more > 0 ) {
			s + s" ($more more)"
		} else s
	}

	private def len(): Int = next match {
		case Some(f) => 1 + f.len()
		case None => 1
	}

	def msgs(): List[String] = this.next match {
		case Some(f) => msg :: f.msgs()
		case _ => List( msg )
	}
}

object SemanticAnalyzer {

	def apply = analyze _

	/**
	 * Applies the different semantic analysis functions simultaneously on
	 * a given AST, returning an AnalysisResult
	 */
	def analyze(term: Term, symbols: SymbolTable): AnalysisResult = {
		term.foldDown[ AnalysisResult ](Success) {
			(r, t) =>
				// perform name analysis on this node
				NameAnalyzer.analyze(t)(symbols) match {
					// perform type analysis only when name analysis has succeeded
					// and combine the results on failure with previous results
					case Success => r and TypeAnalyzer.analyze(t)(symbols)
					case f:Failure => r and f
				}
		}
	}
}

trait Analyzer {

	// import some nice implicit conversions
	// to handle optional terms/records
	import Scope._
	import Record._

	protected def check( v: Boolean, f: Failure ) = if( !v ) f else Success

	protected def get_def_on( term:Term, nss:List[Namespace], name: String )( implicit symbols: SymbolTable ): Option[Term] = {
		// get the scope that belongs to term
		symbols.get_child_scope_of( term ) match {
			// if scope was retrieved succesfully
			// get the definition of name in it
			case Some(s) => get_def( s, List(NSMethod), name)
			case None => throw new CompilerException( s"Could not get scope of term $term" )
		}
	}

	protected def get_def(scope: Scope, nss: List[ Namespace ], name: String): Option[Term] = {
		nss.foldLeft[Option[Term]]( None ) {
			(r, ns ) => r || scope.lookup_lexical(ns, name)
		} match {
			case Some(d) => Some(d)
			case _ => None
		}
	}

	protected def get_def
		( term: Term, nss: List[Namespace], name: String )
		( implicit table: SymbolTable )
		: Option[Term] = {

		// get the scope
		table.get_scope_of(term) match {
			// verify the name is defined in the given scope
			// given any of the provided namespaces
			case Some(s) => get_def( s, nss, name )

			case _ => None
		}
	}
}

object TypeAnalyzer extends Analyzer {

	// import some transparancy between types and typetasks
	import TypeResult._

	/**
	 * The type analyzer is only run on a node if it is succesfully verified by the
	 * name-analyzer
	 *
	 * We can thus assume that definitions of terms exist, if the namespace is known
	 *
	 * @param term
	 * @param nss namespaces to search
	 * @param name
	 * @param table
	 */
	protected def get_sure_def( term: Term, nss: List[Namespace], name: String )( implicit table: SymbolTable ): Term = {
		super.get_def( term, nss, name ) match {
			case Some(t) => t
			case None => throw new CompileException( "Can't get definition of undefined reference." )
		}
	}

	def get_type(term: Term)(implicit symbols: SymbolTable): TypeResult = term match {

		// expression types
		case BinExp(LAnd, _, _) => TBool
		case BinExp(Plus, _, _) => TInt
		case BinExp(Minus, _, _) => TInt
		case BinExp(Mul, _, _) => TInt
		case BinExp(Lt, _, _) => TBool
		case BinExp(Subscript, _, _) => TInt
		case UnExp(Neg, _) => TBool
		case UnExp(Len, _) => TInt
		case IntValue(_) => TInt
		case True => TBool
		case False => TBool
		case NewObject(c) => TClass(c)
		case NewArray(_) => TIntArray

		// references
		case Ref(id) => get_type(get_sure_def(term, List(NSVar, NSField), id))

		// variable declarations
		case VarDecl(typ,id) => typ
		case Field(typ,id) => typ
		case Param(typ,id) => typ

		// method simple type
		case MethodDecl(t,_,_,_,_,_) => t

		case This => get_sure_def(term, List(NSThis), "this") match {
			case ClassDecl(c,_,_,_) => TClass(c)
			case _ => throw new CompileException( "This referred to non-classtype" )
		}

		case Call(inst, name, _) =>
			// first get the type of the obj on which it is called
			val objt = get_type(inst)

			// make sure it is an instance of something
			objt match {

				// if so: lookup the definition of this method
				case Right(TClass(c)) =>
					get_def_on(get_sure_def(term, List(NSClass), c), List(NSMethod), name) match {
						case Some(m) => get_type(m)
						case None => Failure(s"Call to unknown method `$name` on instance of type `$c`")
					}

				case Left(f) => f and Failure(s"Call on non-object of type $objt")
			}

		case _ => Void
	}

	def get_op_type(op: Op): (Type, Type) = op match {
		case Plus => (TInt, TInt)
		case Mul => (TInt, TInt)
		case Minus => (TInt, TInt)
		case Lt => (TInt, TInt)
		case Subscript => (TIntArray, TInt)
		case Neg => (TBool, Void)
		case Len => (TIntArray, Void)
	}

	def analyze(term:Term)(implicit symbols: SymbolTable): AnalysisResult = {

		def teq( left: TypeResult, right: TypeResult ) = ( left, right ) match {
			case ( Right(lt), Right( rt) ) => lt == rt
			case _ => false
		}

		def typeres_to_type( result: TypeResult ) = result match {
			case Left(f) => throw new CompilerException("Tried to use type, but typeresult was a failure")
			case Right(typ) => typ
		}

		term match {

			// declared return type needs to match actual return type
			case MethodDecl(decl_rett, name, _, _, _, retexp) =>
				val actual_rett = get_type(retexp)

				check(
					teq(decl_rett, actual_rett),
					Failure(
						s"Actual return type ${typeres_to_type(actual_rett)} of method `$name` doesn't " +
						s"match declared type ${typeres_to_type(decl_rett)}"
					)
				) <:: actual_rett

			case AssignStmt(varr, exp) =>

				val vart = get_type(get_sure_def(term, List(NSVar, NSField), varr))
				val expt = get_type(exp)

				check(
					teq(vart, expt),
					Failure(
						s"Expected expression of type ${typeres_to_type(vart)}: " +
						s"got ${typeres_to_type(expt)} instead"
					)
				) <:: (vart + expt)

			case ArrayAssignStmt(_, index, value) =>
				val valt = get_type(value)
				val indt = get_type(index)

				// expression assigned to element of int[] should be int
				// and index in arrayassignment should be int
				check(
					teq(TInt, valt),
					Failure(
						s"Int array assignment expects expression of type Int, " +
						s"got ${typetask_to_result(valt)} instead"
					)
				) and check(
					teq(TInt, indt),
					Failure( s"Index must be of type int, got ${typetask_to_result(indt)} instead" )
				) <:: (valt + indt)

			case BinExp( op, left, right ) =>
				val top = get_op_type( op )
				val tleft = get_type(left)
				val tright = get_type(right)
				check(
					teq(top._1, tleft),
					Failure(s"Left operand of $op should be of type $top")
				) and check(
					teq(top._2, tright),
					Failure(s"Right operand of $op should be of type $top")
				) <:: (tleft + tright)

			case UnExp( op, oper ) =>
				val top = get_op_type( op )
				val toper = get_type(oper)
				check(
					teq(top._1, toper),
					Failure(s"Left operand of $op should be of type $top")
				) <:: toper

			case _ => Success
		}
	}
}

object NameAnalyzer extends Analyzer {

	def apply(term: Term, symbols: SymbolTable): AnalysisResult = analyze(term)(symbols)

	private def check_def( term: Term, ns: Namespace, name: String)(implicit table: SymbolTable): Boolean = {
		get_def( term, List(ns), name ).isDefined
	}

	def analyze(terms: List[ Term ])(implicit table: SymbolTable): AnalysisResult = {
		terms.foldLeft[ AnalysisResult ](Success) {
			(r, t) => r and analyze(t)
		}
	}

	def analyze(term: Term)(implicit symbols: SymbolTable): AnalysisResult = {

		term match {

			// verify parent class is defined
			case c@ClassDecl(_, Some(parent), _, _) =>
				check(
					check_def(term, NSClass, parent),
					Failure(s"Woops, missing definition of $parent")
				)

			// verify that types are defined in var decl
			case VarDecl(TClass(c),exp) => check(
				check_def(term, NSClass, c),
				Failure(s"Uhm, missing definition of type `$c`")
			)

			// verify that types are defined in field decl
			case Field(TClass(c), exp) => check(
				check_def(term, NSClass, c),
				Failure(s"Uhm, missing definition of type `$c`")
			)

			// verify that all names in expressions are declared
			case Ref(x) => check(
				check_def(term, NSVar, x) || check_def(term, NSField, x) ,
				Failure(s"Referring to undeclared variable $x")
			)

			case NewObject(c) => check(
				check_def(term, NSClass, c),
				Failure(s"Missing definition of class $c found.")
			)

			case Param(TClass(c),_) => check(
				check_def(term, NSClass, c),
				Failure(s"Missing definition of class $c found.")
			)

			case _ => Success
		}
	}
}
} //end of analysis package
} //end of minijava package
