package s99

import Solutions._
import scala.collection.immutable.Range
import scala.util.Random

trait ListsSolutions {

  def last[T](list: List[T]): T = list.last
  def penultimate[T](list: List[T]): T = list.takeRight(2).head
  def nth[T](n: Int, list: List[T]): T = list(n)
  def length[T](list: List[T]): Int = list.length
  def reverse[T](list: List[T]): List[T] = list.reverse
  def isPalindrome[T](list: List[T]): Boolean = list.take(list.length/2) equals list.takeRight(list.length/2).reverse
  /*{
	  if (list.isEmpty) true
	  else if (list.head != list.last) false 
	  else isPalindrome(list.tail.init)
  }*/
  
  def flatten(list: List[Any]): List[Any] = list.flatMap({
    case xs: List[_] => flatten(xs)
    case x: Any => List(x)
  })
      
  def compress[T](list: List[T]): List[T] = {
    def compressIter[T](xs: List[T], acc: List[T]): List[T] = {
      if (xs.isEmpty) acc
      else compressIter(xs.dropWhile(x => x == xs.head), acc :+ xs.head)
    }
    compressIter[T](list, List())
  }
  
  def pack[T](list: List[T]): List[List[T]] = {
    def packIter[T](xs: List[T], acc: List[List[T]]): List[List[T]] = {
      if (xs.isEmpty) acc
      else packIter(xs.dropWhile(x => x == xs.head), acc :+ xs.takeWhile(x => x == xs.head))
    }
    packIter[T](list, List())
  }
  
  def encode[T](list: List[T]): List[(Int, T)] = pack(list) map { ls => (ls.length, ls.head) }
  def encodeModified[T](list: List[T]): List[Any] = pack(list) map { ls => if (ls.length > 1) (ls.length, ls.head) else ls.head }
  def decode[T](list: List[(Int, T)]): List[T] = list flatMap { case (n, x) => List().padTo(n, x) }
  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    def encodeIter[T](xs: List[T], acc: List[(Int, T)]): List[(Int, T)] = {
      if (xs.isEmpty) acc
      else encodeIter(xs.dropWhile(x => x == xs.head), acc :+ (xs.takeWhile(x => x == xs.head).length, xs.head))
    }
    encodeIter(list, List())
  }
  
  def duplicate[T](list: List[T]): List[T] = list flatMap {x => List(x,x)}
  def duplicateN[T](n: Int, list: List[T]): List[T] = list flatMap { x => List().padTo(n, x) }
  def drop[T](n: Int, list: List[T]): List[T] = {
    def dropIter(xs: List[T], acc: List[T]): List[T] = {
      if (xs.isEmpty) acc 
      else dropIter(xs.drop(n), acc ++ xs.take(n - 1))
    }
    
    dropIter(list, List())
  }
  
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = list splitAt n
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = list.slice(i, j)
  def rotate[T](n: Int, list: List[T]): List[T] = {
    if (n < 0) list.takeRight(-n) ++ list.dropRight(-n)
    else list.drop(n) ++ list.take(n)
  }
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = (list.patch(i, List(), 1), list(i))
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = list.take(i) ++ List(t) ++ list.drop(i)
  def range[T](i: Int, j: Int): List[Int] = (i to j) toList
  def randomSelect[T](n: Int, list: List[T]): List[T] = Random.shuffle(list) take n
  def lotto[T](i: Int, j: Int): List[Int] = Random.shuffle(1 to j toList) take i
  def randomPermute[T](list: List[T]): List[T] = Random.shuffle(list)
  def combinations[T](n: Int, list: List[T]): List[List[T]] = list.combinations(n).toList
  def group3[T](list: List[T]): List[List[List[T]]] = {
    combinations(2, list).flatMap(
        xs => combinations(3, list.diff(xs)).map(ys => List(xs, ys))
        ).map(xss => xss :+ list.diff(xss.flatten))
  }
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = {
    combinations(ns(0), list).flatMap(
        xs => combinations(ns(1), list.diff(xs)).map(ys => List(xs, ys))
        ).map(xss => (xss :+ list.diff(xss.flatten)).filter(xs => !xs.isEmpty))
  }
  def lsort[T](list: List[List[T]]): List[List[T]] = list.sortWith((xs,ys) => xs.length < ys.length)
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    list.sortWith(
        (xs,ys) => list.count(zs => zs.length == xs.length) < list.count(zs => zs.length == ys.length)
        )
  }
  
}

