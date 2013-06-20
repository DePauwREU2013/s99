package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = {
      def aux(i: Int): Boolean =
        (i * i > n) || ((n % i != 0) && aux(i + 2))
        
      (n % 2 != 0) && aux(3)
    }
    def isCoprimeTo(n: Int): Boolean = ???
    def totient: Int = ???
    def improvedTotient: Int = ???
    def primeFactors: List[Int] = ???
    def primeFactorMultiplicity: List[(Int, Int)] = ???
    def primeFactorMultiplicityMap: Map[Int, Int] = ???
    def listPrimesinRange(r: Range): List[Int] = ???
    def goldbach: (Int, Int) = ???

  }

  def primes: Stream[Int] = ???
  def gcd(m: Int, n: Int): Int = ???
  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

}
