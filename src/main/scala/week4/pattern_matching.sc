object pattern_matching {

  trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
      case Prod(e1, e2) => e1.eval * e2.eval
    }

    def show: String = {

      def parentheses(e: Expr) = {
        e match {
          case Sum(_, _) => "(" + e.show + ")"
          case _ => e.show
        }
      }

      this match {
        case Number(n) => n.toString
        case Var(x) => x
        case Sum(l, r) => l.show + " + " + r.show
        case Prod(l, r) => parentheses(l) + " * " + parentheses(r)
      }
    }
  }

  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  Number(1)
  Sum(Number(1), Number(4)).eval
  Prod(Number(2), Number(3)).eval

  Number(1).show
  Sum(Number(1), Number(4)).show
  Prod(Number(2), Number(3)).show

  Sum(Prod(Number(2), Var("x")), Var("y")).show
  Prod(Sum(Number(2), Var("x")), Var("y")).show
  Prod(Var("x"), Sum(Number(2), Var("y"))).show
  Prod(Sum(Number(2), Var("x")), Sum(Number(4), Var("y"))).show
}