package examples

trait Tret {
  def bum(x: Int, i: Int): String
  def bang(i: Int): String
}

@main
object MyApp {
  def bla(i: Int) = s"number: $i"

  //@mapargs(i => i.toString: Int => String)
  // @mapargs(_.toString: Int => String)
  @mapargs(bla: Int => String)
  class Ditte extends Tret {
    def bum(x: Int, s: String) = s * x
    def bang(s: String) = s + "!"
  }

  val ditte = new Ditte
  println(ditte.bang(1))
  println(ditte.bum(3, 12))
}
