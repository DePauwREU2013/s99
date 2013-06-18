package s99

import Solutions.???

trait LogicAndCodesSolutions { outer =>

  implicit def extendBoolean(a: Boolean): ExtendedBoolean = ExtendedBoolean(a)
  case class ExtendedBoolean(a: Boolean) {
    def and (b: =>Boolean): Boolean = outer.and (a, b)
    def or  (b: =>Boolean): Boolean = outer.or  (a, b) 
    def nand(b: =>Boolean): Boolean = outer.nand(a, b)
    def nor (b: =>Boolean): Boolean = outer.nor (a, b)
    def xor (b: =>Boolean): Boolean = outer.xor (a, b)
    def impl(b: =>Boolean): Boolean = outer.impl(a, b)
    def equ (b: =>Boolean): Boolean = outer.equ (a, b)

  }
  
  def and(a: Boolean,  b: =>Boolean): Boolean = if (a) b else false
  def or(a: Boolean,   b: =>Boolean): Boolean = if (a) true else b
  def nand(a: Boolean,  b: =>Boolean): Boolean = ! (a and b)
  def nor(a: Boolean,  b: =>Boolean): Boolean = if (a) false else !b
  def xor(a: Boolean,  b: =>Boolean): Boolean = (a or b) and !(a and b)
  def impl(a: Boolean,  b: =>Boolean): Boolean = if (!a) true else b
  def equ(a: Boolean,  b: =>Boolean): Boolean = (a impl b) and (b impl a)
  def not(a: Boolean) = !a
  
  def table2(f: (Boolean, Boolean) => Boolean): String = {
    "A     B     result" +
    outerProduct(List(true, false)).map(xs => xs match { 
      case a :: b :: Nil => "\n" + a.toString.padTo(6, ' ') + b.toString.padTo(6, ' ') +  f(a,b).toString.padTo(6, ' ')
      case _ => ""
    }).mkString
  }
  
  def grayStream: Stream[List[String]] = {
    Stream.iterate(List(""))(xs => xs.map('0'+_) ++: xs.map('1'+_).reverse)
  }
  
  def gray(n: Int): List[String] = grayStream(n)
  
  def huffman(list: List[(String,  Int)]): List[(String, String)] = ???

  def cross[A](xs:List[A], ys: List[A]): List[List[A]] = {
    for {
      x <- xs
      y <- ys
    } yield List(x,y)
  }
  
  def outerProduct[A](xs:List[A]): List[List[A]] = cross(xs, xs)
}
