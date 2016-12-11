package examples

trait Dat {
  def bum(x: Int, i: Int): String
  def bang(i: Int): String
}

//@main
object MyApp extends App {
  def bla(i: Int) = s"number: $i"

  //@mapargs(i => i.toString: Int => String)
  //@mapargs(_.toString: Int => String)
  @mapargs(bla: Int => String)
  class Ditte extends Dat {
    def bum(x: Int, s: String) = s * x
    def bang(s: String) = s + "!"
  }

  val ditte = new Ditte
  println(ditte.bang(1))
  println(ditte.bum(3, 12))

  trait CopyT {
    val a: Int
    def copy(a: Int): CopyT
  }

  @derive(copy)
  case class CopyC(a: Int) extends CopyT

  @derive(Case)
  class Caese(val a: Int, val b: Long, val c: Double, val d: Float, val e: String)

  val caese = Caese(1, 2, 3.0, 4.0f, "5")
  println(caese)

  @derive((x,y) => toString)
  trait Tret {
    val x: Int
    val y: Int = 3
    val z: String = "bla"
  }

  val tret = new Tret { val x = 13 }
  println(tret)

  @derive((x,y) => (toString, apply), y => copy, unapply, hashCode, equals)
  class Clars(val x: Int, val y: Int)
  object Clars { def apply(x: Int) = new Clars(x, 1) }

  val clars = Clars(13, 14)
  clars match {
    case Clars(x,y) => println(x + y)
  }
  println(clars)
  println(clars.copy(y = 0))

  @derive(Equality, Factory, Product, copy, toString)
  class Clazz(val x: Int, val y: Int)

  val clazz = Clazz(13, 14)
  println(clazz)
  println(clazz.copy(x = 0))
  println(clazz == Clazz(13,14))
  println(clazz == Clazz(13,13))
  println(clazz.productElement(1))

  case class Root(foo: Foo)
  case class Foo(bar: Bar)
  case class Bar(s: String)
  class X extends ZoomRW[Root] {
    @gen val handler = zoomRW(_.foo.bar)
  }
  //zoomRW(_.foo.bar)((m,v) => m.copy(foo = m.foo.copy(bar = v)))
}
