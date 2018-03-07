object list_ho_functions {

  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }

  def scaleListByMap(xs: List[Double], factor: Double): List[Double] =
    xs map (x => x * factor)

  scaleList(List(1.0, 2.0, 3.0, 4.0), 2)
  scaleListByMap(List(1.0, 2.0, 3.0, 4.0), 2)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

  def squareListByMap(xs: List[Int]): List[Int] = xs map (x => x * x)

  squareList(List(1, 2, 3, 4))
  squareListByMap(List(1, 2, 3, 4))

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }

  def posElemsByFilter(xs: List[Int]): List[Int] = xs filter (x => x > 0)

  posElems(List(-1, 0, 4, -1, 7))
  posElemsByFilter(List(-1, 0, 4, -1, 7))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x.head, x.length))

  pack(List("a", "a", "a", "b", "c", "c", "a"))
  encode(List("a", "a", "a", "b", "c", "c", "a"))

  val nums = List(6, -5, -2, 0, 1, 4, 7)
  val fruits = List("apple", "orange", "pear", "banana", "cherry")

  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)
}