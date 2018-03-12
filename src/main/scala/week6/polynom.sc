object polynom {

  class Polynom(val terms: Map[Int, Double]) {

    def + (other: Polynom) = new Polynom(terms ++ (other.terms map addTerm))

    private def addTerm(term: (Int, Double)) = {
      val (degree, coeff) = term
      degree -> (coeff + terms(degree))
    }

    override def toString = {
      val termStrings =
        for ((pos, value) <- terms.toList.sorted.reverse) yield value + "x^" + pos
      termStrings mkString " + "
    }
  }

  def Polynom(bindings: (Int, Double)*) =
    new Polynom(bindings.toMap withDefaultValue 0)

  val p1 = Polynom(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = Polynom(0 -> 3.0, 3 -> 7.0)

  p1 + p2
}
