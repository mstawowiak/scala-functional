object list {

  val fruit = List("apples", "oranges", "pears")
  val nums = 1 :: 2 :: 3 :: 4 :: Nil
  val diag3 = List(List(1, 0 ,0), List(0, 1 ,0), List(0, 0 ,1))
  val empty = List()
  val empty2 = Nil

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
  }

  isort(List(3, 9, 2, 4, 12, 1))

}