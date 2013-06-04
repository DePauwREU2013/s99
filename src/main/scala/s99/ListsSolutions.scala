package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = list match {
    case x :: Nil => x
    case _ :: xs => last(xs)
    case _ => sys.error("oops")
  }

  def penultimate[T](list: List[T]): T = list match {
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
    case _ => sys.error("oops")
  }

  def nth[T](n: Int, list: List[T]): T = (n, list) match {
    case (0, x :: _) => x
    case (_, _ :: xs) => nth(n - 1, xs)
    case _ => sys.error("oops")
  }

  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case _ :: xs => 1 + length(xs)
  }

  def reverse[T](list: List[T]): List[T] = {
    def aux(list: List[T], accum: List[T]): List[T] = list match {
      case Nil => accum
      case x :: xs => aux(xs, x :: accum)
    }

    aux(list, Nil)
  }

  def isPalindrome[T](list: List[T]): Boolean = list == reverse(list)

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (x : List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case x :: xs => x :: flatten(xs)
  }

  def compress[T](list: List[T]): List[T] = list match {
    case y :: x :: xs =>
      if (y == x) compress(x :: xs) else y :: compress(x :: xs)
    case _ => list
  }

  def pack[T](list: List[T]): List[List[T]] = {
    def aux(list: List[T], accum: List[T]): List[List[T]] = (list, accum) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => accum :: Nil
      case (x :: xs, Nil) => aux(xs, x :: Nil)
      case (x :: xs, y :: _) =>
        if (x == y) aux(xs, x :: accum) else accum :: aux(xs, x :: Nil)
    }

    aux(list, Nil)
  }

  def encode[T](list: List[T]): List[(Int, T)] =
    pack(list) map {xs => (length(xs), xs.head)}

  def encodeModified[T](list: List[T]): List[Any] =
    pack(list) map {xs => if (length(xs) == 1) xs.head else (length(xs), xs.head)}

  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case (n, x) :: nxs => {
      def aux(n: Int): List[T] = n match {
        case 0 => decode(nxs)
        case _ => x :: aux(n - 1)
      }

      aux(n)
    }
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case x :: xs => {
      def aux(xs: List[T], n: Int): List[(Int, T)] = xs match {
        case Nil => (n, x) :: Nil
        case y :: ys => if (x == y) aux(ys, n + 1) else (n, x) :: encodeDirect(xs)
      }

      aux(xs, 1)
    }
  }

  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  def duplicateN[T](n: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => {
      def aux(nn: Int): List[T] = nn match {
        case 0 => duplicateN(n, xs)
        case _ => x :: aux(nn - 1)
      }

      aux(n)
    }
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    def aux(nn: Int, list: List[T]): List[T] = (nn, list) match {
      case (_, Nil) => Nil
      case (1, _ :: xs) => aux(n, xs)
      case (_, x :: xs) => x :: aux(nn - 1, xs)
    }

    aux(n, list)
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = n match {
    case 0 => (Nil, list)
    case _ => {
      val (first, second) = split(n - 1, list.tail)
      (list.head :: first, second)
    }
  }

  def slice[T](i: Int, j: Int, list: List[T]): List[T] = {
    val (_, list2) = split(i, list)
    val (result, _) = split(j - i, list2)
    result
  }

  def rotate[T](n: Int, list: List[T]): List[T] = {
    val len = list.length
    val nn = if (n >= 0) n % len else len - 1 - ((len - 1 - n) % len)
    val (first, second) = split(nn, list)
    second ::: first
  }

  def removeAt[T](i: Int, list: List[T]): (List[T], T) = (i, list) match {
    case (0, x :: xs) => (xs, x)
    case (_, x :: xs) => {
      val (ys, y) = removeAt(i - 1, xs)
      (x :: ys, y)
    }
    case _ => sys.error("oops")
  }

  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = (i, list) match {
    case (0, _) => t :: list
    case (_, x :: xs) => x :: insertAt(t, i - 1, xs)
    case _ => sys.error("oops")
  } 

  def range(i: Int, j: Int): List[Int] = if (i > j) Nil else i :: range(i + 1, j)

  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

