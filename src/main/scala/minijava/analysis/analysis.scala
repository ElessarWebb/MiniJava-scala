package minijava.analysis

import minijava.parser._

import scala.collection.{mutable,immutable}
import scala.annotation.tailrec
import minijava.utils.{StackTrace, CompileException}

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
	implicit def rec_to_term( rec: Record ): Term = rec.definition
	implicit def rec_to_type( rec: Record ): Type = rec.typ
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

object NameAnalysis {

	def apply(term: Term, symbols: SymbolTable): AnalysisResult = analyze(term)(symbols)

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

	private def check( v: Boolean, f: Failure ) = if( !v ) f else Success

	def analyze(term: Term)(implicit symbols: SymbolTable): AnalysisResult = {
		term match {

			// verify parent class is defined
			case c@ClassDecl(_, Some(parent), _, _) =>
				check(
					check_def(term, NSClass, parent),
					Failure(s"Woops, missing definition of $parent")
				) and analyze(c.children())

			// verify that types are defined in var decl
			case VarDecl(TClass(c),exp) => check(
				check_def(term, NSClass, c),
				Failure(s"Uhm, the type `$c` is not defined")
			)

			// verify that types are defined in field decl
			case Field(TClass(c), exp) => check(
				check_def(term, NSClass, c),
				Failure(s"Uhm, the type `$c` is not defined")
			)

			// verify that all names in expressions are declared
			case Ref(x) => check(
				check_def(term, NSVar, x),
				Failure(s"Referring to undeclared variable $x")
			)

			case _ => analyze( term.children() )
		}
	}

}
