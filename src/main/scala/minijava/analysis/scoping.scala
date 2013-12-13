package minijava.analysis

import minijava.parser._

import scala.collection.{mutable,immutable}
import minijava.utils.CompileException

sealed class Namespace
case object NSClass extends Namespace
case object NSMethod extends Namespace
case object NSField extends Namespace
case object NSVar extends Namespace
case object NSThis extends Namespace

case class Record(
	definition: Term,
	typ: Type
	)

object Record {
	implicit def opt_rec_to_term( rec: Option[Record] ): Option[Term] = rec match {
		case Some(r) => Some(r.definition)
		case None => None
	}
	implicit def rec_to_term( rec: Record ): Term = rec.definition
	implicit def rec_to_type( rec: Record ): Type = rec.typ
}

/**
 * Packs an implicit def to cope with Options of Terms
 */
object Scope {

	class TermWrapper(t: Option[Term]) {
		/**
		 * Evaluates to left term if left is defined, and right otherwise.
		 * Supports short circuit evaluation
		 *
		 * @param right not evaluated in case left is defined
		 */
		def ||(right: => Option[Term]): Option[Term] = {
			if( t.isEmpty ) right else t
		}
	}

	implicit def termWrapper(t: Option[Term]) = new TermWrapper(t)
}

class Scope(
	val parent: Option[ Scope ],
	val scopes: List[ Namespace ],

	// represents the actual (local) symbol-table
	val symbols: mutable.Map[ (Namespace, String), Record ] = new mutable.HashMap()
	) {

	// TODO: How would you deal with updating scopes when using immutability?
	def add( ns: Namespace, id: String, record: Record ): Scope = {
		symbols += Tuple2((ns, id), record )
		this
	}

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
	val term_in_scope: immutable.HashMap[ Term, Scope ] = new immutable.HashMap()
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
		val nextctx = current.add(ns, id, Record(term, typ))
		new SymbolTable(root, nextctx, term_in_scope + Tuple2(term, nextctx))
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
		val rootctx = new Scope( None, Nil )
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
			val clsst = parent match {
				case Some(p) => TClass(p)
				case None => TTop
			}

			// recurse on children
			build(
				fields ::: methods,
				ctx .declare(NSClass, id, c, clsst)
					.declare(NSThis, "this", c, TClass(id) )
					.enter_scope(List(NSField, NSMethod))
			).leave_scope()

		case f@Field(typ,name) => ctx.declare(NSField, name, f, typ)

		case m@MethodDecl(typ,name,params,vardecls,body,retexp) =>
			// lookup the enclosing class
			val self = ctx.current.lookup_lexical( NSThis, "this" ) match {
				case Some(Record(ClassDecl(cname,_,_,_),_)) => cname
				case _ => throw new CompileException( s"Could not find enclosing class of method `$name`" )
			}

			// declare the method in the parent scope
			// and enter method scope
			val decl =
				ctx	.declare(NSMethod, s"$self.$name", m, typ)
					.enter_scope(List(NSVar))

			// recurse on children
			// and leave the method scope again
			build( params ::: vardecls ::: ( body :+ retexp ), decl ).leave_scope()

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

