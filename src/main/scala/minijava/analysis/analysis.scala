package minijava.analysis {

import minijava.parser._
import minijava.utils.CompileException

case class AnalysisException(msg:String) extends CompileException(msg)

object AnalysisResult {
	implicit def opt_to_result[A]( opt: Option[A] ): AnalysisResult[A] = opt match {
		case None => Failure( "Error" )
		case Some(x) => Success(x)
	}

	def apply[T](x: => T) =
		try Success(x)
		catch {
			case AnalysisException(msg) => Failure(msg, None)
		}
}

abstract sealed class AnalysisResult[+T] {

	/**
	 * Combines two analysis results into a single one
	 *
	 * 	left	right	result
	 * 	----------------------
	 * 	Suc(a)	Suc(b)	Suc(b)
	 * 	Suc(a)	Fail(m)	Fail(m)
	 * 	Fail(m)	Suc(b)	Fail(m)
	 * 	Fail(m1)Fail(m2)chain of right failure llist and then left llist
	 */
	def &[U >: T](right: AnalysisResult[U]): AnalysisResult[U] = (this,right) match {
		// success is temporary
		case (l@Success(_), r) => r
		// failure prevails
		case (l@Failure(msg,f), r@Success(_)) => Failure(msg,f)
		// concat the chains of failures
		case (l@Failure(_,_), r@Failure(_,_)) => r.foldRight(l) { (r,x) => Failure( x.msg, Some(r)) }
	}

	/**
	 * Combines two analysis results into a single one
	 *
	 * 	left	right	result
	 * 	----------------------
	 * 	Suc(a)	Suc(b)	Suc(a)
	 * 	Suc(a)	Fail(m)	Suc(a)
	 * 	Fail(m)	Suc(b)	Suc(b)
	 * 	Fail(m1)Fail(m2)Fail(m2)
	 *
	 * @param right
	 * @return
	 */
	def |[U >: T](right: AnalysisResult[U]): AnalysisResult[U] = (this,right) match {
		case (l:Success[T],_) => l
		case (l:Failure,r) => r
	}

	def flatMap[U](f: T => AnalysisResult[U]): AnalysisResult[U] = this match {
		case Success(t) => f(t)
		case fail: Failure => fail
	}

	def map[U](f: T => U): AnalysisResult[U] = this match {
		case Success(t) => AnalysisResult(f(t))
		case fail: Failure => fail
	}
}

case class Success[T](result: T) extends AnalysisResult[T]

case class Failure(msg: String, next: Option[ Failure ] = None) extends AnalysisResult[Nothing] {

	implicit def failToOpt(f: Failure) = Some(f)

	override def toString: String = {
		val s = s"$msg"
		val more = len() - 1

		if( more > 0 ) {
			s + s" ($more more)"
		} else s
	}

	private def len(): Int = foldLeft(0) {(r,e) => r + 1 }

	final def foldLeft[A](z: A)(f: (A,Failure) => A): A = next match {
		// apply to self, then to child
		case Some(fail) => fail.foldLeft(f(z, this))(f)
		case None => f(z,this)
	}

	final def foldRight[A](z: A)(f: (A,Failure) => A): A = next match {
		// apply to child, then to self
		case Some(fail) => f(fail.foldLeft(z)(f), this)
		case None => f(z,this)
	}

	def msgs() = foldLeft[List[String]](Nil) { (r, e) => e.msg :: r }
}

object SemanticAnalyzer {

	def apply = analyze _

	/**
	 * Applies the different semantic analysis functions simultaneously on
	 * a given AST, returning an AnalysisResult
	 */
	def analyze(term: Term, symbols: SymbolTable) = {
		term.foldDown[AnalysisResult[Boolean]](Success(true)) {
			(r, t) =>
				// perform name analysis on this node
				NameAnalyzer.analyze(t)(symbols) match {
					// perform type analysis only when name analysis has succeeded
					// and combine the results on failure with previous results
					case l:Success[Boolean] => r.flatMap { _ => TypeAnalyzer.analyze(t)(symbols) }
					case l:Failure => l & r
				}
		}
	}
}

trait Analyzer {

	// import some nice implicit conversions
	// to handle optional terms/records
	import Scope._
	import Record._

	protected def check( v: Boolean, f: Failure ): AnalysisResult[Boolean] = if( !v ) f else Success(true)

	protected def get_def_on( term:Term, nss:List[Namespace], name: String )
		( implicit symbols: SymbolTable ): Option[Term] = {
		// get the scope that belongs to term
		symbols.get_child_scope_of( term ) match {
			// if scope was retrieved succesfully
			// get the definition of name in it
			case Some(s) => get_def( s, List(NSMethod), name)
			case None => throw new CompileException( s"Could not get scope of term $term" )
		}
	}

	protected def get_def(scope: Scope, nss: List[ Namespace ], name: String): Option[Term] = {
		nss.foldLeft[Option[Term]]( None ) {
			(r, ns ) => r || scope.lookup_lexical(ns, name)
		}
	}

	protected def get_def
		( term: Term, nss: List[Namespace], name: String )
		( implicit table: SymbolTable )
		: Option[Term] = {

		// get the scope
		// verify the name is defined in the given scope
		// given any of the provided namespaces
		table.get_scope_of(term).flatMap { get_def(_, nss, name) }
	}
}

object TypeAnalyzer extends Analyzer {

	// some transparancy between option type and AnalysisResult
	import AnalysisResult._

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
			case None => throw new AnalysisException( "Can't get definition of undefined reference." )
		}
	}

	implicit def type_to_result(x:Type) = Success(x)

	def get_type(term: Term)(implicit symbols: SymbolTable): AnalysisResult[Type] = term match {

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
			// and ensure it's a classy thing
			get_type(inst).flatMap[Type] {
				case TClass(c) =>
					val cdef = get_sure_def(term,List(NSClass), c)

					// try get the definition of the method
					// ensuring that we did find the definition of the metho
					(	get_def_on(cdef, List(NSMethod), name)
					 	| Failure( s"Could not resolve method $name on object of class $c" )
					).flatMap { get_type }

				// if not classy, we need a reprimand
				case t => Failure(s"Call on non-object of type $t")
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
		case LAnd => (TBool, TBool)
	}

	private def assert_type( left: Type, right: Type, fail_msg: String ): Boolean = {
		if( left != right )
			throw AnalysisException(fail_msg)
		else true
	}

	def analyze(term:Term)(implicit symbols: SymbolTable): AnalysisResult[Boolean] = {

		term match {

			// declared return type needs to match actual return type
			case MethodDecl(decl_rett, name, _, _, _, retexp) =>
				get_type(retexp).flatMap { actual_rett =>
					check(
						decl_rett == actual_rett,
						Failure(
							s"Actual return type $actual_rett of method `$name` doesn't " +
							s"match declared type $decl_rett"
						)
					)
				}

			case AssignStmt(varr, exp) =>

				for {
					vart <- get_type(get_sure_def(term, List(NSVar, NSField), varr))
					expt <- get_type(exp)
				} yield {
					assert_type(vart, expt,
						s"Expected expression of type $vart: " +
						s"got $expt instead"
					)
				}

			case ArrayAssignStmt(_, index, value) =>

				for {
					valt <- get_type(value)
					indt <- get_type(index)
				} yield {
					// expression assigned to element of int[] should be int
					// and index in arrayassignment should be int
					assert_type( valt, TInt,
						s"Int array assignment expects expression of type Int, " +
						s"got $valt instead"
					) && assert_type( indt, TInt,
						s"Index must be of type int, got $indt instead"
					)
				}

			case BinExp( op, left, right ) =>
				val top = get_op_type( op )

				for {
					tleft <- get_type(left)
					tright <- get_type(right)
				} yield {
					(assert_type(top._1, tleft, s"Left operand of $op should be of type $top")
					&& assert_type(top._2, tright, s"Right operand of $op should be of type $top"))
				}

			case UnExp( op, oper ) =>
				val top = get_op_type( op )
				get_type(oper).flatMap { opert =>
					check(
						opert == top._1,
						Failure(s"Left operand of $op should be of type $top")
					)
				}

			case _ => Success(true)
		}
	}
}

object NameAnalyzer extends Analyzer {

	def apply(term: Term, symbols: SymbolTable) = analyze(term)(symbols)

	private def check_def( term: Term, ns: Namespace, name: String)(implicit table: SymbolTable): Boolean = {
		get_def( term, List(ns), name ).isDefined
	}

	def analyze(terms: List[ Term ])(implicit table: SymbolTable):AnalysisResult[Boolean] = {
		terms.foldLeft[AnalysisResult[Boolean]] (Success(true)) {
			(r, t) => r.flatMap[Boolean] { _ => analyze(t) }
		}
	}

	def analyze(term: Term)(implicit symbols: SymbolTable): AnalysisResult[Boolean] = {

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

			case _ => Success(true)
		}
	}
}
} //end of analysis package
