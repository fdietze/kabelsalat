package examples

import scala.meta._

case class Value(name: String, tpe: Type.Arg)
case class Method(method: String, values: Seq[Value])

object Derive {
  def apply(typeName: String, classArgs: Option[Seq[Value]], meths: Seq[Method]): List[Defn.Def] = {
    meths.map {
      case Method("toString", values) =>
        val names = values.map(v => Term.Name(v.name)).toList
        q"""override def toString: String = $typeName + (..$names)"""
      case Method("copy", values) =>
        val names = classArgs.toSeq.flatten.map(v => Term.Name(v.name)).toList
        val params = values.map(v => Term.Param(List.empty, Term.Name(v.name), Some(v.tpe), Some(Term.Name(v.name)))).toList
        val construct = Ctor.Name(typeName)
        q"""def copy(..$params) = new $construct(..$names)"""
      case meth => abort(s"unknown method: $meth")
    }.toList
  }
}

class deriveFor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val definitions = args map {
      case Term.Function(params, names) => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(names) => names.map(_.toString) // TODO: Term.Name
      }, params.map(_.toString))
      case arg => abort(s"unexpected argument: $arg")
    }

    def valuesInBody(body: Seq[Stat]) = body.collect {
      case v: Decl.Val => v.pats.map(pat => Value(pat.name.value, v.decltpe))
      case v: Defn.Val => v.pats.collect { case p: Pat.Var.Term => Value(p.name.value, v.decltpe.get) }
    }.flatten

    def valuesInArgs(args: Seq[Term.Param]) = args.collect {
      case Term.Param(_, Term.Name(name), Some(tpe), _) => Value(name, tpe)
    }

    def genMethods(typeName: String, args: Option[Seq[Value]], defs: Seq[Value]) = definitions.flatMap { case (methods, members) =>
      val allDefs = args.toSeq.flatten ++ defs
      val values = members.map { member =>
        allDefs.find(_.name == member) match {
          case Some(v) => v
          case None => abort(s"missing value definition for member '$member' in type '$typeName'")
        }
      }

      Derive(typeName, args, methods.map(m => Method(m, values)))
    }

    defn match {
      case q"..$classMods trait $className extends ..$classParents { ..$body }" =>
        val derived = genMethods(className.value, None, valuesInBody(body))
        q"..$classMods trait $className extends ..$classParents { ..$body; ..$derived }"
      case q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body }" =>
        val derived = genMethods(className.value, Some(valuesInArgs(classArgs)), valuesInBody(body))
        q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body; ..$derived }"
    }
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
    val meths = methods.map(m => Method(m, values))
    val derived = Derive(className.value, Some(values), meths)
    q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body; ..$derived }"
  }
}
