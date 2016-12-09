package examples

import scala.meta._
import scala.collection.immutable.Seq

case class Value(name: Term.Name, tpe: Type.Arg)

sealed trait ValueSelection
case object ValuesFromArgs extends ValueSelection
case class ValuesByName(names: Seq[String]) extends ValueSelection
case class MethodConfig(method: String, selection: ValueSelection)

sealed trait ModuleDef { val name: String }
case class ClassDef(defn: Defn.Class, companion: Option[Defn.Object]) extends ModuleDef { val name = defn.name.value }
case class TraitDef(defn: Defn.Trait, companion: Option[Defn.Object]) extends ModuleDef { val name = defn.name.value }

object Derive {
  def valuesInTempl(templ: Template) = templ.stats.toSeq.flatten.collect {
    case v: Decl.Val => v.pats.map(pat => Value(pat.name, v.decltpe))
    case v: Defn.Val => v.pats.collect { case p: Pat.Var.Term => Value(p.name, v.decltpe.get) }
  }.flatten

  def valuesInCtor(ctor: Ctor.Primary) = ctor.paramss.flatten.collect {
    case Term.Param(_, Term.Name(name), Some(tpe), _) => Value(Term.Name(name), tpe)
  }

  def valuesInModule(module: ModuleDef) = module match {
    case ClassDef(c, _) => valuesInTempl(c.templ) ++ valuesInCtor(c.ctor)
    case TraitDef(t, _) => valuesInTempl(t.templ)
  }

  def templWithMethods(templ: Template, methods: Seq[Defn.Def]) = {
    val newStats = templ.stats.toSeq.flatten ++ methods
    templ.copy(stats = Some(Seq(newStats: _*)))
  }

  def deriveModule(module: ModuleDef, configs: Seq[MethodConfig]): Stat = {
    val methods = genMethods(module, configs)
    val (defn, companion) = module match {
      case ClassDef(c, comp) => (c.copy(templ = templWithMethods(c.templ, methods)), comp)
      case TraitDef(t, comp) => (t.copy(templ = templWithMethods(t.templ, methods)), comp)
    }

    companion.map(c => q"$defn; $c").getOrElse(defn)
  }

  def genMethods(module: ModuleDef, configs: Seq[MethodConfig]): Seq[Defn.Def] = configs.map { conf =>
    val values = selectValues(module, conf.selection)
    mapMethod(module, values).applyOrElse(conf.method, (m: String) => abort(s"unknown derive method: $m"))
  }

  def selectValues(module: ModuleDef, selection: ValueSelection): Seq[Value] = (module, selection) match {
    case (_, ValuesByName(names)) =>
      val definedVals = valuesInModule(module)
      names.map(name => definedVals.find(_.name.value == name) match {
        case Some(v) => v
        case None => abort(s"missing value definition for member '$name' in type '${module.name}'")
      })
    case (ClassDef(c, _), ValuesFromArgs) => valuesInCtor(c.ctor)
    case (m, ValuesFromArgs) => abort("cannot select arguments from constructor: type '${m.name}' is not a class")
  }

  def mapMethod(module: ModuleDef, values: Seq[Value]): PartialFunction[String, Defn.Def] = {
    case "toString" =>
      val names = values.map(_.name).toList
      q"""override def toString: String = ${module.name} + (..$names)"""
    case "copy" => module match {
      case ClassDef(c, _) =>
        val params = values.map(v => Term.Param(List.empty, v.name, Some(v.tpe), Some(v.name))).toList
        val names = c.ctor.paramss.map(_.map(v => Term.Name(v.name.value))).toList
        val ctor = Ctor.Name(c.name.value)
        q"""def copy(..$params) = new $ctor(...$names)"""
      case _ => abort(s"cannot generate method 'copy': type '${module.name}' is not a class")
    }
  }
}

class derive extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val configs = args.map {
      case Term.Function(params, names) => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(vals) => vals map {
          case Term.Name(name) => name
          case arg => abort(s"unexpected argument in tuple: $arg")
        }
      }, ValuesByName(params.map(_.toString)))
      case Term.Name(name) => (List(name), ValuesFromArgs)
      case arg => abort(s"unexpected argument: $arg")
    } flatMap { case (ms, sel) => ms.map(m => MethodConfig(m, sel)) }

    val module = defn match {
      case t: Defn.Trait => TraitDef(t, None)
      case c: Defn.Class => ClassDef(c, None)
      case d => d.children match {
        case (t: Defn.Trait) :: (o: Defn.Object) :: Nil => TraitDef(t, Some(o))
        case (t: Defn.Class) :: (o: Defn.Object) :: Nil => ClassDef(t, Some(o))
        case _ => abort(s"unexpected annotation: $defn")
      }
    }

    Derive.deriveModule(module, configs)
  }
}
