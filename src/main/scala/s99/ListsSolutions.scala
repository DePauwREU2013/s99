package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = T match {
    case Nil => sys.error("error.")
    case tail :: Nil => tail
    case head :: tail => last[T]
  }
  def penultimate[T](list: List[T]): T = list match{
    case Nil => sys.error("error")
    case head::tail::moretail => tail
    case head::tail => penultimate(tail)
  }
  def nth[T](n: Int, list: List[T]): T = list(n)
  def length[T](list: List[T]): Int = list match{
    case Nil => 0
    case head::tail => 1 + length(tail)
  }
  def reverse[T](list: List[T]): List[T] = list match{
    case Nil => sys.error("ERROR")
    case head::Nil => head
    case head::tail => reverse(tail)::head
  }
  def isPalindrome[T](list: List[T]): Boolean = list match{
    case Nil => True
    case head::Nil => True
    case head::tail =>
      if head == tail
  }
  def flatten(list: List[Any]): List[Any] = ???
  def compress[T](list: List[T]): List[T] = ???
  def pack[T](list: List[T]): List[List[T]] = ???
  def encode[T](list: List[T]): List[(Int, T)] = ???
  def encodeModified[T](list: List[T]): List[Any] = ???
  def decode[T](list: List[(Int, T)]): List[T] = ???
  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???
  def duplicate[T](list: List[T]): List[T] = ???
  def duplicateN[T](n: Int, list: List[T]): List[T] = ???
  def drop[T](n: Int, list: List[T]): List[T] = ???
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???
  def rotate[T](n: Int, list: List[T]): List[T] = ???
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

