package minijava.analysis

import minijava.parser._
import minijava.utils.CompileException

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

	def and(right: AnalysisResult) = right match {
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

	protected def get_def
		( term: Term, nss: List[Namespace], name: String )
		( implicit table: SymbolTable )
		: Option[Term] = {

		// get the scope
		table.get_scope_of(term) match {
			// verify the name is defined in the given scope
			// given any of the provided namespaces
			case Some(s) =>
				nss.foldLeft[Option[Term]]( None ) {
					(r, ns ) => r || s.lookup_lexical(ns, name)
				} match {
					case Some(d) => Some(d)
					case _ => None
				}

			case _ => None
		}
	}
}

object TypeAnalyzer extends Analyzer {

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

	def get_type(term: Term)(implicit symbols: SymbolTable): Type = term match {

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

		// references
		case Ref(id) => get_type(get_sure_def(term, List(NSVar, NSField), id))

		// variable declarations
		case VarDecl(typ,id) => typ
		case Field(typ,id) => typ
		case Param(typ,id) => typ

		// method simple type
		case MethodDecl(_,_,_,_,_, ret_exp) => get_type(ret_exp)

		case _ => Void
	}

	def analyze(term:Term)(implicit symbols: SymbolTable): AnalysisResult = {

		def teq( left: Type, right: Type ) = left == right

		term match {

			case MethodDecl(decl_rett, name, _, _, _, retexp) =>
				val actual_rett = get_type( retexp )
				check(
					teq( decl_rett, actual_rett),
					Failure( s"Actual return type $actual_rett of method `$name` doesn't match declared type $decl_rett" )
				)

			case AssignStmt(varr, exp) =>
				val vart = get_type(get_sure_def(term, List(NSVar, NSField), varr))
				val expt = get_type(exp)
				check(
					teq(vart, expt),
					Failure( s"Expected expression of type $vart: got $expt instead" )
				)

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
