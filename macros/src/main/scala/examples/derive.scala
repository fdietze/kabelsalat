package examples

import scala.meta._

case class Value(name: String, tpe: Type.Arg)

object Derive {
  def apply(typeName: String, methods: Seq[String], values: Seq[Value]): List[Defn.Def] = {
    val valueNames = values.map(v => Term.Name(v.name)).toList
    methods.map {
      case "toString" =>
        q"""override def toString: String = $typeName + (..$valueNames)"""
      case "copy" =>
        val params = values.map(v => Term.Param(List.empty, Term.Name(v.name), Some(v.tpe), Some(Term.Name(v.name)))).toList
        val construct = Ctor.Name(typeName)
        q"""def copy(..$params) = new $construct(..$valueNames)"""
      case meth => abort(s"unknown method: $meth")
    }.toList
  }
}

class deriveFor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val (methods, members) = args match {
      case Term.Function(params, names) :: Nil => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(names) => names.map(_.toString) // TODO: Term.Name
      }, params.map(_.toString))
      case arg => abort(s"unexpected argument: $arg")
    }

    val q"..$classMods trait $className extends ..$classParents { ..$body }" = defn

    val defs = body.collect {
      case v: Decl.Val => v.pats.map(pat => Value(pat.name.value, v.decltpe))
      case v: Defn.Val => v.pats.collect { case p: Pat.Var.Term => Value(p.name.value, v.decltpe.get) }
    }.flatten

    val values = members.map { member =>
      defs.find(_.name == member) match {
        case Some(v) => v
        case None => abort(s"missing value definition for member '$member' in type '$className'")
      }
    }

    val derived = Derive(className.value, methods, values)
    q"..$classMods trait $className extends ..$classParents { ..$body; ..$derived }"
  }
}

class derive extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val methods = args map {
      case q"${name: Term.Name}" => name.toString
      case arg => abort(s"unexpected argument: $arg")
    }

    val q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body }" = defn
    val values = classArgs.map(v => Value(v.name.value, v.decltpe.get))
    val derived = Derive(className.value, methods, values)
    q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body; ..$derived }"
  }
}
