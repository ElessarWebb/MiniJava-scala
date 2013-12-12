package minijava.analysis

import minijava.parser._

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

object SemanticAnalysis {

	def apply(term: Term, symbols: SymbolTable): AnalysisResult = {
		term.foldDown[ AnalysisResult ](Success) {
			// perform name analysis on this node
			// and combine the results
			(r, t) => r and NameAnalysis.analyze(t)(symbols)
		}
	}
}

object NameAnalysis {

	def apply(term: Term, symbols: SymbolTable): AnalysisResult = analyze(term)(symbols)

	private def check( v: Boolean, f: Failure ) = if( !v ) f else Success

	def check_def( term: Term, ns: Namespace, name: String)(implicit table: SymbolTable): Boolean = {
		// get the scope
		table.get_scope_of(term) match {
			// verify the name is defined in the given scope
			case Some(s) => s.lookup_lexical(ns, name) match {
				case Some(d) => true
				case _ => false
			}
			case _ => true
		}
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
