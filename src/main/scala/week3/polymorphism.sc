import week3.{List, Cons, Nil}

object polymorphism {

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  singleton(1)
  singleton(true)

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException("n is out of range")
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }

  nth(0, singleton(2))

  val listA = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

  nth(0, listA)
  nth(1, listA)
  nth(2, listA)

  nth(4, listA)
}