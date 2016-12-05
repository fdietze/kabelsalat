package examples

import scala.meta._

class derive extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val deriveMethods = args map {
      case q"${name: Term.Name}" => name.toString
      case arg => abort(s"unexpected argument: $arg")
    }

    println(args)
    val q"..$classMods class $className(..$values) extends ..$classParents { ..$body }" = defn
    val valueArgs = values.map(v => Term.Name(v.name.value))
    val derived = deriveMethods map {
      case "toString" => q"""override def toString: String = ${className.value} + (..$valueArgs)"""
      case meth => abort(s"unknown method: $meth")
    }

    q"..$classMods class $className(..$values) extends ..$classParents { ..$body; ..$derived }"
  }
}
