package examples

import scala.meta._
import scala.collection.immutable.Seq

trait ZoomRW[S] {
  // def zoomRW[S,U](get: S => U)(set: (S, U) => S)(implicit feq: FastEq[_ >: U]) = ???
  def zoomRW[U](get: S => U)(set: (S, U) => S) = ???
}

object Gen {
  def nameToParam(name: Term.Name): Term.Param = {
    Term.Param(Seq.empty, name, None, None)
  }

  def params(select: Term.Select): Seq[Term.Param] = select match {
    case Term.Select(inner: Term.Select, name: Term.Name) => params(inner) :+ nameToParam(name)
    case Term.Select(Term.Placeholder(), name: Term.Name) => List(nameToParam(name))
    case _ => abort(s"unexpected select: $select")
  }

  def setter(select: Term.Select): Term.Function = {
    val name = select.name
    val ps = params(select)
    println(ps)
    q"(m,v) => ???"
  }
}

class gen extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    println(defn.structure)
    val generated = defn match {
      case q"val $name = $func($getter)" => getter match {
        case sel: Term.Select =>
          val setter = Gen.setter(sel)
          val valName = Pat.Var.Term(Term.Name(name.toString))
          q"val $valName = $func($getter)($setter)"
        case expr => abort(s"invalid getter: $expr")
      }
      case annottee => abort(s"cannot annotate: $annottee")
    }
    println(generated)
    generated
  }
}
