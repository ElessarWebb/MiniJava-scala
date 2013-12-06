package minijava.analysis

import minijava.utils.StackTrace
import minijava.parser._

import scala.collection.immutable.HashMap

sealed class Namespace
case object NSClass extends Namespace
case object NSMethod extends Namespace
case object NSField extends Namespace
case object NSVar extends Namespace

class Scope(
	val parent: Option[ Scope ],
	val scopes: List[ Namespace ],

	// represents the actual (local) symbol-table
	val symbols: HashMap[ (Namespace, String), Type ] = new HashMap()
	) extends Immutable {

	/* Look up a name in the local scope
	 */
	def lookup_local(ns: Namespace, id: String): Option[ Type ] = this.symbols.get((ns, id))

	/* Look up a name, wrt to the scoping rules for the provided namespace
	 */
	def lookup_lexical(ns: Namespace, id: String): Option[ Type ] = {
		lookup_local(ns, id) match {
			// if we have a local match, we're done
			case m: Some[Type] => m
			case _ =>
				// check if the namespace is scoped by us
				// else lookup in parent scope
				if ( !this.scopes.contains(ns) )
					lookup_parent(ns, id)
				else
					None
		}
	}

	def add( ns: Namespace, id: String, t: Type): Scope =
		new Scope(parent, scopes, symbols + Tuple2((ns, id), t))

	private def lookup_parent(ns: Namespace, id: String): Option[ Type ] = {
		this.parent match {
			case Some(p) => p.lookup_lexical(ns, id)
			case _ => None
		}
	}
}

abstract sealed class AnalysisResult {
	def and( right: AnalysisResult ): AnalysisResult
}

case object Success extends AnalysisResult {
	def and( right: AnalysisResult ) = right match {
		case Success => Success
		case Failure => Failure
	}
}

case object Failure extends AnalysisResult {
	def and( right: AnalysisResult ) = Failure
}

object SemanticAnalysis {

	def check_type(ctx: Scope, root: Term): AnalysisResult = root match {
		case Program( main, classes ) => {
			// forward declare all classes
			val ctxext = classes.foldLeft(ctx) {
				(r, c) => ctx.add(NSClass, c.id, Void)
			}

			// now check the child terms
			check_type(ctx, main) and check_type( ctx, classes )
		}

		case ClassDecl(_,_,_,_) => Failure
		case _ => Success
	}

	def check_type(ctx: Scope, roots: List[Term]): AnalysisResult =
		roots.foldLeft[AnalysisResult]( Success ) { (r,el) => r and check_type( ctx, el )}

	def apply(root: Term, symbols: HashMap[ String, Term ]): AnalysisResult = Success
}
