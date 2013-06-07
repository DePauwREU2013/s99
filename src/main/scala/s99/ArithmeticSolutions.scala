package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = (2 to Math.sqrt(n).toInt).forall(n % _ > 0)
    
    def isCoprimeTo(m: Int): Boolean = gcd(n,m) == 1
    
    def totient: Int = (1 to n).count(n isCoprimeTo _)
    
    // Hmm...My implementation of this function essentially matches his
    // However, it still runs slower than the one above, 
    // causing it to fail the specs test. :/
    def improvedTotient: Int = primeFactorMultiplicity map {
        case (p, r) => (p-1)*Math.pow(p, r-1).toInt
      } product
    
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
    
    def primeFactorMultiplicity: List[(Int, Int)] = {
      for {
        p <- primeFactors.distinct
      } yield (p, primeFactors.count(_==p))
    }
    
    def primeFactorMultiplicityMap: Map[Int, Int] = {
      primeFactors.groupBy(_.toInt).transform((key, ls) => ls.length)
    }
    
    def goldbach: (Int, Int) =  
      (2 until n).filter(p => p.isPrime && (n - p).isPrime).map(p => (p,n-p)).head

  }
  
  def primes: Stream[Int] = Stream.from(1).filter(_.isPrime)
  
  def gcd(m: Int, n: Int): Int = {
    if (m > n) gcd(n, m)
    else if (n % m == 0) m
    else gcd(n % m, m)
  }
  
  def listPrimesinRange(r: Range): List[Int] = r.filter(_.isPrime).toList
  
  def printGoldbachList(r: Range): List[String] = {
    r.filter(m => m % 2 == 0).map(m => m + " = " + m.goldbach._1 + " + " + m.goldbach._2).toList
  }
  
  // The description of expected behavior is not clear enough.
  // Apparently the tests author believes that the 'limited'
  // version should only include numbers which ONLY have pairs
  // > the limit provided. What about numbers that have multiple
  // decompositions? We should just ignore them?
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = {
    for {
      composite <- r.filter(a => a % 2 == 0).toList
      p <- listPrimesinRange(limit to r.end)
      if (composite - p) > limit && (composite - p).isPrime
    } yield composite + " = " + p + " + " + (composite - p)
  }

}
