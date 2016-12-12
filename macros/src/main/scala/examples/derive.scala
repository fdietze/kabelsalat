package examples

import scala.meta._
import scala.collection.immutable.Seq

case class Value(name: Term.Name, tpe: Type.Arg)

case class Patch(methods: Seq[Method], parents: Seq[Ctor.Call] = Seq.empty)
object Patch {
  import Method._

  val Product = Ctor.Name("Product")

  def apply(name: String, values: Seq[Value]): Either[String, Patch] = Some(name).collect {
    case "canEqual" => Patch(CanEqual :: Nil) //TODO: should not be able to create with valueselection: (x,y) => canEqual
    case "toString" => Patch(ToString(values) :: Nil)
    case "unapply" => Patch(Unapply(values) :: Nil)
    case "hashCode" => Patch(HashCode(values) :: Nil)
    case "equals" => Patch(Equals(values) :: CanEqual :: Nil)
    case "copy" => Patch(Copy(values) :: Nil)
    case "apply" => Patch(Apply(values) :: Nil)
    case "Product" => Patch(ProductIterator :: ProductPrefix :: ProductArity(values) :: ProductElement(values) :: Nil, Product :: Nil)
    case "Equality" => Patch(Equals(values) :: CanEqual :: HashCode(values) :: Nil)
    case "Factory" => Patch(Apply(values) :: Unapply(values) :: Nil)
    case "Case" => Patch(Apply(values) :: Unapply(values) :: ToString(values) :: Copy(values) :: HashCode(values) :: Equals(values) :: CanEqual :: ProductIterator :: ProductPrefix :: ProductArity(values) :: ProductElement(values) :: Nil, Product :: Nil)
  }.map(p => Right(p)).getOrElse(Left(s"unknown patch: $name"))
}

sealed trait Method
object Method {
  case class ToString(values: Seq[Value]) extends Method
  case class Copy(values: Seq[Value]) extends Method
  case class Apply(values: Seq[Value]) extends Method
  case class Unapply(values: Seq[Value]) extends Method
  case class HashCode(values: Seq[Value]) extends Method
  case class Equals(values: Seq[Value]) extends Method
  case class ProductElement(values: Seq[Value]) extends Method
  case class ProductArity(values: Seq[Value]) extends Method
  case object ProductIterator extends Method
  case object ProductPrefix extends Method
  case object CanEqual extends Method
}

sealed trait ModuleDef { val name: String }
case class ClassDef(defn: Defn.Class, companion: Option[Defn.Object]) extends ModuleDef { val name = defn.name.value }
case class TraitDef(defn: Defn.Trait, companion: Option[Defn.Object]) extends ModuleDef { val name = defn.name.value }

sealed trait GenMethod
case class InstanceMethod(method: Defn.Def) extends GenMethod
case class CompanionMethod(method: Defn.Def) extends GenMethod

sealed trait ValueSelection
case object AutoValues extends ValueSelection
case class ValuesByName(names: Seq[String]) extends ValueSelection
case class PatchConfig(name: String, selection: ValueSelection)

object EitherHelper {
  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = s collectFirst { case Left(l) => Left(l) } getOrElse Right(s collect { case Right(x) => x })
}

object ValueSelection {
  import EitherHelper._

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

  def select(module: ModuleDef, selection: ValueSelection): Either[String, Seq[Value]] = (module, selection) match {
    case (_, ValuesByName(names)) =>
      val definedVals = valuesInModule(module)
      sequence(names.map(name => definedVals.find(_.name.value == name) match {
        case Some(v) => Right(v)
        case None => Left(s"missing value definition for member '$name' in type '${module.name}'")
      }))
    case (ClassDef(c, _), AutoValues) => Right(valuesInCtor(c.ctor))
    case (m, AutoValues) => Left("cannot select arguments from constructor: type '${m.name}' is not a class")
  }
}

object Derive {
  import EitherHelper._

  def updateTemplate(templ: Template, methods: Seq[Defn.Def], parents: Seq[Ctor.Call]) = {
    //TOOD: check for existing...
    val newStats = templ.stats.toSeq.flatten ++ methods
    templ.copy(stats = Some(Seq(newStats: _*)), parents = parents)
  }

  def updateCompanion(name: Type.Name, companion: Option[Defn.Object], methods: Seq[Defn.Def], parents: Seq[Ctor.Call]): Option[Defn.Object] = {
    companion.map { comp =>
      Some(comp.copy(templ = updateTemplate(comp.templ, methods, parents)))
    } getOrElse {
      if (methods.isEmpty) None else Some(q"object ${Term.Name(name.value)} extends ..$parents { ..$methods }")
    }
  }

  def parseConfigs(module: ModuleDef, configs: Seq[PatchConfig]): Either[String, Seq[Patch]] = sequence(configs.map { config =>
    ValueSelection.select(module, config.selection) match {
      case Left(l) => Left(l)
      case Right(values) => Patch(config.name, values)
    }
  })

  def deriveModule(module: ModuleDef, configs: Seq[PatchConfig]): Either[String, Stat] = parseConfigs(module, configs) match {
    case Left(l) => Left(l)
    case Right(patches) =>
      val methods = patches.flatMap(_.methods).distinct
      val parents = patches.flatMap(_.parents).distinct
      sequence(methods.map(genMethod(module))).right.map { generatedMethods =>
        val instMethods = generatedMethods collect { case InstanceMethod(m) => m }
        val compMethods = generatedMethods collect { case CompanionMethod(m) => m }
        val (defn, companion) = module match {
          case ClassDef(c, comp) =>
            (c.copy(templ = updateTemplate(c.templ, instMethods, parents)), updateCompanion(c.name, comp, compMethods, Seq.empty))
          case TraitDef(t, comp) =>
            (t.copy(templ = updateTemplate(t.templ, instMethods, parents)), updateCompanion(t.name, comp, compMethods, Seq.empty))
        }

        companion.map(c => q"$defn; $c").getOrElse(defn)
      }
  }

  //TODO: fresh variables everywhere
  //TODO: reuse existing canEqual
  def genMethod(module: ModuleDef)(method: Method): Either[String, GenMethod] = method match {
    case Method.ToString(values) =>
      val names = values.map(_.name).toList
      Right(InstanceMethod(q"override def toString: String = ${module.name} + (..$names)"))
    case Method.Copy(values) => module match {
      case c: ClassDef =>
        val params = values.map(v => Term.Param(List.empty, v.name, Some(v.tpe), Some(v.name))).toList
        val names = c.defn.ctor.paramss.map(_.map(v => Term.Name(v.name.value))).toList
        val ctor = Ctor.Name(c.defn.name.value)
        Right(InstanceMethod(q"def copy(..$params) = new $ctor(...$names)"))
      case _ => Left(s"cannot generate method 'copy': type '${module.name}' is not a class")
    }
    case Method.Apply(values) => module match {//TODO missing args?
      case c: ClassDef =>
        val params = values.map(v => Term.Param(List.empty, v.name, Some(v.tpe), None)).toList
        val names = c.defn.ctor.paramss.map(_.map(v => Term.Name(v.name.value))).toList
        val ctor = Ctor.Name(c.defn.name.value)
        Right(CompanionMethod(q"def apply(..$params) = new $ctor(...$names)"))
      case _ => Left(s"cannot generate method 'apply': type '${module.name}' is not a class")
    }
    case Method.Unapply(values) => //TODO: missing vals?
      val tpe = Type.Name(module.name)
      val vals = values.map(v => q"t.${v.name}").toList
      val res = if (values.size == 1) vals.head else q"(..$vals)"
      Right(CompanionMethod(q"def unapply(that: $tpe) = Option(that).map(t => $res)"))
    case Method.HashCode(values) =>
      val accs = values.map {
        case Value(name, Type.Name("Int")) => q"$name"
        case Value(name, Type.Name("Long")) => q"scala.runtime.Statics.longHash($name)"
        case Value(name, Type.Name("Double")) => q"scala.runtime.Statics.doubleHash($name)"
        case Value(name, Type.Name("Float")) => q"scala.runtime.Statics.floatHash($name)"
        case Value(name, _) => q"scala.runtime.Statics.anyHash($name)"
      }.map(mix => q"acc = scala.runtime.Statics.mix(acc, $mix)")
      Right(InstanceMethod(q"""override def hashCode: Int = {
        var acc: Int = -889275714
        ..$accs
        scala.runtime.Statics.finalizeHash(acc, ${accs.size})
      }"""))
    case Method.Equals(values) =>
      val tpe = Type.Name(module.name)
      val comparisons = values.map(v => q"(t.${v.name} == $tpe.this.${v.name})")
      val condition = comparisons.fold(q"$tpe.this.canEqual(that)")((a,b) => q"$a && $b")
      Right(InstanceMethod(q"""override def equals(that: Any) = $tpe.this.eq(that.asInstanceOf[Object]) || (that match {
        case (t: $tpe) => $condition
        case _ => false
      })"""))
    case Method.CanEqual =>
      val tpe = Type.Name(module.name)
      Right(InstanceMethod(q"def canEqual(that: Any) = $tpe.this.isInstanceOf[$tpe]"))
    case Method.ProductPrefix =>
      Right(InstanceMethod(q"override def productPrefix: String = ${module.name}"))
    case Method.ProductArity(values) =>
      Right(InstanceMethod(q"def productArity: Int = ${values.size}"))
    case Method.ProductIterator =>
      val tpe = Type.Name(module.name)
      Right(InstanceMethod(q"override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any]($tpe.this)"))
    case Method.ProductElement(values) =>
      val tpe = Type.Name(module.name)
      val cases = values.zipWithIndex.map { case (v,i) => p"case $i => $tpe.this.${v.name}" }
      Right(InstanceMethod(q"""def productElement(n: Int): Any = n match {
        ..case $cases
        case _ => throw new IndexOutOfBoundsException(n.toString())
      }"""))
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
      case Term.Name(name) => (List(name), AutoValues)
      case arg => abort(s"unexpected argument: $arg")
    } flatMap { case (ms, sel) => ms.map(m => PatchConfig(m, sel)) }

    println(configs)
    val module = defn match {
      case t: Defn.Trait => TraitDef(t, None)
      case c: Defn.Class => ClassDef(c, None)
      case d => d.children match {
        case (t: Defn.Trait) :: (o: Defn.Object) :: Nil => TraitDef(t, Some(o))
        case (t: Defn.Class) :: (o: Defn.Object) :: Nil => ClassDef(t, Some(o))
        case _ => abort(s"unexpected annotation: $defn")
      }
    }

    println(module)
    Derive.deriveModule(module, configs) match {
      case Left(err) => abort(err)
      case Right(t) => println(t); t
    }
  }
}

//TODO: case class:
//  - class/trait toString depend on Product? scala.runtime.ScalaRunTime._toString(Foo.this);
//  - companion with apply implements AbstractFunctionN[in..., Type], Serializable
//  - companion has toString = typename
