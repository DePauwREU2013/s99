package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = (2 to Math.sqrt(n).toInt).forall(n % _ > 0)
    
    def isCoprimeTo(m: Int): Boolean = gcd(n,m) == 1
    
    def totient: Int = (1 to n).count(n isCoprimeTo _)
    
    def improvedTotient: Int = ???
    
    def primeFactors: List[Int] = {
      def primeFactorsIter(m: Int, fs: List[Int]): List[Int] = {
        if (m == 1) fs
        else { 
          val nextFactor: Int = primes.tail.takeWhile(_<= m).filter(m % _ == 0).head
          primeFactorsIter(m / nextFactor, fs :+ nextFactor)
        }
      }
      primeFactorsIter(n, List())
    }
    
    def primeFactorMultiplicity: List[(Int, Int)] = ???
    def primeFactorMultiplicityMap: Map[Int, Int] = ???
    def listPrimesinRange(r: Range): List[Int] = ???
    def goldbach: (Int, Int) = ???

  }

  def primes: Stream[Int] = Stream.from(1).filter(_.isPrime)
  
  def gcd(m: Int, n: Int): Int = {
    if (m > n) gcd(n, m)
    else if (n % m == 0) m
    else gcd(n % m, m)
  }
  
  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

}
