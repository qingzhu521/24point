import scala.util.{Success, Try}

trait Expr {
  def left: Expr

  def right: Expr

  def res: Try[Double]
}

case class Value(v: Double) extends Expr {

  override def left = null

  override def right = null

  override def res = Try(v)
}

case class Cal(c: String, l: Expr, r: Expr) extends Expr {

  override def left: Expr = l

  override def right: Expr = r

  override def res: Try[Double] = c match {
    case "+" => for (x <- l.res; y <- r.res) yield x + y
    case "-" => for (x <- l.res; y <- r.res) yield x - y
    case "*" => for (x <- l.res; y <- r.res) yield x * y
    case "/" => for (x <- l.res; y <- r.res) yield x / y
  }
}

object Main {
  val ops = Array("+", "-", "*", "/");

  def dfs(x: List[Expr]): List[Expr] = {
    if (x.size == 1) {
      return List(x.head)
    } else {
      val totallist: List[List[Expr]] = for {expset <- for {
        i <- x
        j <- x
        if i != j
        op <- ops
      } yield x.filter(e => !(e eq i) && !(e eq j)) ::: List(new Cal(op, i, j))
      } yield dfs(expset)
      totallist.reduce((a, b) => a ::: b)
    }
  }

  def dfshelper(i: Array[Int]) {
    val f = for {n <- i}
      yield new Value(n)
    f.foreach(print)
    println()
    val l = dfs(f.toList)
    for (i <- l if i.res == Success(24.0)) {
      println(i)
      println(i.res)
    }
  }

  def main(args: Array[String]) = {
    val x = Array(1, 5, 5, 5)
    dfshelper(x)
  }
}
