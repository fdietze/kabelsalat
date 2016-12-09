package examples

trait Dat {
  def bum(x: Int, i: Int): String
  def bang(i: Int): String
}

@main
object MyApp {
  def bla(i: Int) = s"number: $i"

  //@mapargs(i => i.toString: Int => String)
  // @mapargs(_.toString: Int => String)
  @mapargs(bla: Int => String)
  class Ditte extends Dat {
    def bum(x: Int, s: String) = s * x
    def bang(s: String) = s + "!"
  }

  val ditte = new Ditte
  println(ditte.bang(1))
  println(ditte.bum(3, 12))

  @derive((x,y) => toString)
  trait Tret {
    val x: Int
    val y: Int = 3
    val z: String = "bla"
  }

  val tret = new Tret { val x = 13 }
  println(tret)

  @derive((x,y) => toString, y => copy)
  class Clars(x: Int, y: Int)
  object Clars { def apply(x: Int) = new Clars(x, 1) }

  val clars = new Clars(13, 14)
  println(clars)
  println(clars.copy(y = 0))

  @derive(toString, x => copy)
  class Clazz(x: Int, y: Int)

  val clazz = new Clazz(13, 14)
  println(clazz)
  println(clazz.copy(x = 0))
}
