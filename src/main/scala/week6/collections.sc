object collections {

  val nums = Vector(1, 2, 3, -5)
  val fruits = Vector("apple", "orange", "pear", "pinneaple")

  val xs = Array(1, 2, 3, -5)
  xs map (x => x + 1)

  val s = "Hello World"
  s filter (c => c.isUpper)

  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val pairs = List(1,2,3) zip fruits
  pairs.unzip

  s flatMap (c => List('.', c))
  nums flatMap (n => List(n + 1))

  nums.sum
  nums.max
  nums.min

  fruits sortWith (_.length < _.length)
  fruits.sorted

  fruits groupBy (_.head)

  //Range
  val r1: Range = 1 until 5
  val r2: Range = 1 to 5
  1 to 10 by 3
  6 to 1 by -2

  def combination(N: Int, M: Int): Seq[(Int, Int)] =
    (1 to M) flatMap (x => ((1 to N)) map (y => (x, y)))

  combination(4, 2)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum
  //(xs zip ys).map(xy => xy._1 * xy._2).sum

  scalarProduct(Vector(1, 2, 3), Vector(1, 2, 3))

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

  isPrime(7)
  isPrime(8)

}