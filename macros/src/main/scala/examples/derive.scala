package examples

import scala.meta._

object Derive {
  def apply(typeName: String, methods: Seq[String], values: Seq[String]): List[Defn.Def] = {
    val args = values.map(Term.Name(_)).toList
    methods.map {
      case "toString" => q"""override def toString: String = $typeName + (..$args)"""
      case meth => abort(s"unknown method: $meth")
    }.toList
  }
}

class deriveFor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val (methods, values) = args match {
      case Term.Function(params, names) :: Nil => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(names) => names.map(_.toString) // TODO: Term.Name
      }, params)
      case arg => abort(s"unexpected argument: $arg")
    }

    val q"..$classMods trait $className extends ..$classParents { ..$body }" = defn
    val derived = Derive(className.value, methods, values.map(_.name.value))
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

    val q"..$classMods class $className(..$values) extends ..$classParents { ..$body }" = defn
    val derived = Derive(className.value, methods, values.map(_.name.value))
    q"..$classMods class $className(..$values) extends ..$classParents { ..$body; ..$derived }"
  }
}
