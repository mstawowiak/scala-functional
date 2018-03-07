object list {

  val fruits = List("apples", "oranges", "pears")
  val nums = 1 :: 2 :: 3 :: 4 :: Nil
  val diag3 = List(List(1, 0 ,0), List(0, 1 ,0), List(0, 0 ,1))
  val empty = List()
  val empty2 = Nil

  val pair = ("key", 123)
  val (label, value) = pair


  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }
  last(List('a', 'b', 'c', 'd'))

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("last of init list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }
  init(List('a', 'b', 'c', 'd'))

  def concat[T](xs: List[T], ys: List[T]): List[T] =  xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }
  concat(List('a', 'b', 'c', 'd'), List('e','f','g','h'))

  def reverse[T](xs: List[T]): List[T] =  xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }
  reverse(List('a', 'b', 'c', 'd'))

  def removeAt[T](xs: List[T], n: Int) =
    if (n >= xs.length) xs
    else (xs take n) ::: (xs drop (n + 1))

  removeAt(List('a', 'b', 'c', 'd'), 4) // List(a, b, c, d)
  removeAt(List('a', 'b', 'c', 'd'), 1) // List(a, c, d)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case y :: ys => y match {
      case z: List[Any] => flatten(z) ++ flatten(ys)
      case _ => y :: flatten(ys)
    }
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}