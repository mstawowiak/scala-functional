object list_reduction {

  def sumRecursive(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sumRecursive(ys)
  }

  def productRecursive(xs: List[Int]): Int = xs match {
    case Nil => 1
    case y :: ys => y * productRecursive(ys)
  }

  //def sumReduceLeft(xs: List[Int]) =     (0 :: xs) reduceLeft ((x, y) => x + y)
  //def productReduceLeft(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)

  def sumReduceLeft(xs: List[Int])     = (0 :: xs) reduceLeft (_ + _)
  def productReduceLeft(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

  def sumFoldLeft(xs: List[Int])     = (xs foldLeft 0) (_ + _)
  def productFoldLeft(xs: List[Int]) = (xs foldLeft 1) (_ * _)

  def sumReduceRight(xs: List[Int])     = (0 :: xs) reduceRight (_ + _)
  def productReduceRight(xs: List[Int]) = (1 :: xs) reduceRight (_ * _)

  def sumFoldRight(xs: List[Int])     = (xs foldRight 0) (_ + _)
  def productFoldRight(xs: List[Int]) = (xs foldRight 1) (_ * _)

  val nums = List(1, 2, 3, 4, 5)

  sumFoldLeft(nums)
  sumReduceLeft(nums)
  sumFoldRight(nums)
  sumReduceRight(nums)
  sumRecursive(nums)

  productFoldLeft(nums)
  productReduceLeft(nums)
  productFoldRight(nums)
  productReduceRight(nums)
  productRecursive(nums)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_)::_)

  mapFun[Int, Int](nums, x => x + 1)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, y) => 1 + y)

  lengthFun(nums)

}