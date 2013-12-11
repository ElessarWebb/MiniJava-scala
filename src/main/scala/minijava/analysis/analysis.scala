package minijava.analysis

import minijava.parser._

import scala.collection.immutable.{HashMap, Map}
import scala.annotation.tailrec

sealed class Namespace
case object NSClass extends Namespace
case object NSMethod extends Namespace
case object NSField extends Namespace
case object NSVar extends Namespace

sealed class Record()

case object Usage extends Record
case class Declaration(
	definition: Term,
	typ: Type
) extends Record

object EmptyScope extends Scope( None, Nil )
class Scope(
	val parent: Option[ Scope ],
	val scopes: List[ Namespace ],

	// represents the actual (local) symbol-table
	val symbols: Map[ (Namespace, String), Record ] = new HashMap()
	) extends Immutable {

	def add( ns: Namespace, id: String, record: Record ): Scope =
		new Scope(parent, scopes, symbols + Tuple2((ns, id), record ))

	/* Look up a name in the local scope
	 */
	def lookup_local(ns: Namespace, id: String): Option[ Record ] = this.symbols.get((ns, id))

	/* Look up a name, wrt to the scoping rules for the provided namespace
	 */
	def lookup_lexical(ns: Namespace, id: String): Option[ Record ] = {
		lookup_local(ns, id) match {
			// if we have a local match, we're done
			case m@Some(_) => m
			case _ =>
				// check if the namespace is scoped by us
				// else lookup in parent scope
				if ( !this.scopes.contains(ns) )
					lookup_parent(ns, id)
				else
					None
		}
	}

	private def lookup_parent(ns: Namespace, id: String): Option[ Record ] = {
		this.parent match {
			case Some(p) => p.lookup_lexical(ns, id)
			case _ => None
		}
	}
}

class SymbolTable(
	val root: Scope,
	val current: Scope,
	val term_in_scope: HashMap[ Term, Scope ] = new HashMap()
	) extends Immutable {

	/**
	 * Register the term as a declaration in the current scope.
	 * And link the term with the same scope immediately.
	 * @param ns Namespace
	 * @param id Name to register
	 * @param term Term that id refers to
	 * @param typ Type of id
	 */
	def declare(ns: Namespace, id: String, term: Term, typ: Type) = {
		val nextctx = current.add(ns, id, Declaration(term, typ))
		new SymbolTable(root, nextctx, term_in_scope + Tuple2(term, current))
	}

	/**
	 * Link the given term to the current scope
	 * @param term
	 */
	def link(term: Term) = {
		new SymbolTable(root, current, term_in_scope + Tuple2(term, current))
	}

	/**
	 * Create a new scope under the current scope and set it as current
	 * @param scopes namespaces to scop
	 * @return
	 */
	def enter_scope(scopes: List[ Namespace ]) = new SymbolTable(
		root,
		new Scope(Some(current), scopes),
		term_in_scope
	)

	@throws[ IllegalArgumentException ]
	def leave_scope() = current.parent match {
		case Some(p) => new SymbolTable(
			root,
			p,
			term_in_scope
		)
		case _ => throw new IllegalArgumentException("Cannot leave top scope")
	}

	def get_scope_of( term: Term ): Option[Scope] = this.term_in_scope.get( term )
}

object SymbolTable {

	def build( root: Term ): SymbolTable = {
		val rootctx = EmptyScope
		build( root, new SymbolTable( rootctx, rootctx ))
	}

	def build( root: Term, ctx: SymbolTable ): SymbolTable = root match {

		case p@Program( main, classes ) =>
			val ctxtop = ctx.link(p)

			// first recurse into main, then into class defs
			// pass the ctx through
			classes.foldLeft(build(main, ctxtop)) {
				(r, c) => {
					build(c, r)
				}
			}

		case c@ClassDecl(id,parent,fields,methods) =>
			// recurse on children
			build(
				fields ::: methods,
				ctx .declare(NSClass, id, c, Void)
					.enter_scope(List(NSField, NSMethod))
			)

		case f@Field(typ,name) => ctx.declare(NSField, name, f, typ)

		case m@MethodDecl(typ,name,params,vardecls,body,retexp) =>
			// declare the method in the parent scope
			// and enter method scope
			val decl =
				ctx	.declare(NSMethod, name, m, typ)
					.enter_scope(List(NSVar))

			// recurse on children
			build( params ::: vardecls ::: ( body :+ retexp ), decl )

		case p@Param(typ,name) => ctx.declare(NSVar,name,p,typ)

		case v@VarDecl(typ,name) => ctx.declare(NSVar,name,v,typ)

		// link any other term and recurse on children
		case x => build(x.children(), ctx.link(x))
	}

	def build( roots: List[Term], ctx: SymbolTable ): SymbolTable =
		roots.foldLeft(ctx) {
			(r, el) => build(el, r)
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
