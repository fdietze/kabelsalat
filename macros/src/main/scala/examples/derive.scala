package examples

import scala.meta._

case class Value(name: String, tpe: Type.Arg)
case class Method(method: String, values: Seq[Value])

object Derive {
  def valuesInBody(body: Seq[Stat]) = body.collect {
    case v: Decl.Val => v.pats.map(pat => Value(pat.name.value, v.decltpe))
    case v: Defn.Val => v.pats.collect { case p: Pat.Var.Term => Value(p.name.value, v.decltpe.get) }
  }.flatten

  def valuesInArgs(args: Seq[Term.Param]) = args.collect {
    case Term.Param(_, Term.Name(name), Some(tpe), _) => Value(name, tpe)
  }

  def genMethods(typeName: String, args: Option[Seq[Value]], defs: Seq[Value], desiredDefs: Seq[(Seq[String], Seq[String])]) = {
    desiredDefs.flatMap { case (methods, members) =>
      val allDefs = args.toSeq.flatten ++ defs
      val values = if (members.isEmpty)
        args.toSeq.flatten
      else
        members.map { member =>
          allDefs.find(_.name == member) match {
            case Some(v) => v
            case None => abort(s"missing value definition for member '$member' in type '$typeName'")
          }
        }

      mapMethods(typeName, args, methods.map(m => Method(m, values)))
    }.toList
  }

  private def mapMethods(typeName: String, classArgs: Option[Seq[Value]], meths: Seq[Method]): List[Defn.Def] = {
    meths.map {
      case Method("toString", values) =>
        val names = values.map(v => Term.Name(v.name)).toList
        q"""override def toString: String = $typeName + (..$names)"""
      case Method("copy", values) =>
        if (classArgs.isEmpty) abort(s"cannot generate method 'copy', because type '$typeName' has no constructor")
        val names = classArgs.toSeq.flatten.map(v => Term.Name(v.name)).toList
        val params = values.map(v => Term.Param(List.empty, Term.Name(v.name), Some(v.tpe), Some(Term.Name(v.name)))).toList
        val construct = Ctor.Name(typeName)
        q"""def copy(..$params) = new $construct(..$names)"""
      case meth => abort(s"unknown method: $meth")
    }.toList
  }
}

class derive extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    import Derive._

    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val desiredDefs = args map {
      case Term.Function(params, names) => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(names) => names.map(_.toString) // TODO: Term.Name
      }, params.map(_.toString))
      case Term.Name(name) => (List(name), List.empty)
      case arg => abort(s"unexpected argument: $arg")
    }

    defn match {
      case q"..$classMods trait $className extends ..$classParents { ..$body }" =>
        val derived = genMethods(className.value, None, valuesInBody(body), desiredDefs)
        q"..$classMods trait $className extends ..$classParents { ..$body; ..$derived }"
      case q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body }" =>
        val derived = genMethods(className.value, Some(valuesInArgs(classArgs)), valuesInBody(body), desiredDefs)
        q"..$classMods class $className(..$classArgs) extends ..$classParents { ..$body; ..$derived }"
    }
  }
}
