import math.Ordering

object list_sorting {

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
  }

  isort(List(3, 9, 2, 4, 12, 1))

  def msortInt(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def mergeInt(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
          case (xs, Nil) => xs
          case (Nil, ys) => ys
          case (x :: xs1, y :: ys1) =>
            if (x < y) x :: mergeInt(xs1, ys)
            else y :: mergeInt(xs, ys1)
        }
      val (first, second) = xs splitAt n
      mergeInt(msortInt(first), msortInt(second))
    }
  }

  msortInt(List(3, 9, 2, 4, 12, 1))
  msortInt(List(-3, 12, 2, 4, -40, 11, 2, 15, -9))

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (xs, Nil) => xs
          case (Nil, ys) => ys
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }

  msort(List(3, 9, 2, 4, 12, 1))
  msort(List("orange", "apple", "pear", "banana"))(Ordering.String)
}