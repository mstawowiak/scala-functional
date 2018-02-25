object decomposition {

  trait Expr {
    def eval: Int
    def show: String
  }

  class Number(n: Int) extends Expr {
    def eval: Int = n
    def show: String = n.toString
  }

  case class Sum(e1: Expr, e2: Expr) {
    def eval: Int = e1.eval + e2.eval
    def show: String = e1.show + " + " + e2.show
  }

  case class Prod(e1: Expr, e2: Expr) {
    def eval: Int = e1.eval + e2.eval
    def show: String = e1.show + " * " + e2.show
  }

  new Number(1).eval
  Sum(new Number(1), new Number(4)).eval
  Prod(new Number(2), new Number(3)).eval

  new Number(1).show
  Sum(new Number(1), new Number(4)).show
  Prod(new Number(2), new Number(3)).show
}