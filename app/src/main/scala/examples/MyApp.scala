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


  case class Root(foo: Foo)
  case class Foo(bar: Bar)
  case class Bar(s: String)
  class X extends ZoomRW[Root] {
    @gen val handler = zoomRW(_.foo.bar)
  }
  //zoomRW(_.foo.bar)((m,v) => m.copy(foo = m.foo.copy(bar = v)))
}
