package avdongre.scala99.problem31to41

import scala.annotation.tailrec

/**
 * Created by adongre on 3/6/15.
 */
object Arithmetic {
    /* Problem 31
     * Determine whether a given integer number is prime.
     * */
    def isPrime(number: Int): Boolean = {
        require(number > 1)

        def isPrimeHelper(divisor: Int): Boolean = {
            if (divisor == 1)
                true
            else
            if (number % divisor == 0)
                false
            else
                isPrimeHelper(divisor - 1)
        }
        isPrimeHelper(number - 1)
    }

    /* Problem 32
     * Determine the greatest common divisor of two positive
     * integer numbers. Use Euclid's algorithm.
     */
    def gcd(a: Int, b: Int): Int = {
        if (b == 0) a
        else gcd(b, a % b)
    }

    /* Problem 33
    * (*) Determine whether two positive integer numbers are coprime.
    * Two numbers are coprime if their greatest common divisor equals 1.
    */

    def coprime(a: Int, b: Int): Boolean = {
        if (gcd(a, b) == 1) true
        else
            false
    }

    /* Problem 34
     * (**) Calculate Euler's totient function phi(m).
     *  Euler's so-called totient function phi(m) is defined as the number of
     *  positive integers r (1 <= r < m) that are coprime to m.
     *  Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
     */
    def totientPhi(number: Int): Int = {
        def totientPhiLoop(curNum: Int, acc: Int): Int = {
            curNum match {
                case 1 => 1 + acc
                case _ => {
                    if (coprime(curNum, number)) {
                        totientPhiLoop(curNum - 1, acc + 1)
                    } else {
                        totientPhiLoop(curNum - 1, acc)
                    }
                }
            }

        }
        totientPhiLoop(number - 1, 0)
    }


    /* Problem 35
     * (**) Determine the prime factors of a given positive integer.
     * Construct a flat list containing the prime factors in ascending order.
     */
    def primeFactors(input: Int): List[Int] = {
        def primeFactorsHelper(prmFctr: Int, number: Int, acc: List[Int]): List[Int] = {
            prmFctr match {
                case x if (prmFctr > number) => acc
                case x if (isPrime(prmFctr) && (number % prmFctr == 0)) => {
                    primeFactorsHelper(prmFctr, number / prmFctr, acc ::: List(prmFctr))
                }
                case _ => primeFactorsHelper(prmFctr + 1, number, acc)
            }
        }
        primeFactorsHelper(3, input, Nil)
    }

    /* Problem 36
     * (**) Determine the prime factors of a given positive integer.
     * Construct a list containing the prime factors and their multiplicity.
     * Example:
     * (prime-factors-mult 315)
     * ((3 2) (5 1) (7 1))
     */
    def primeFactorsMult(input: Int): List[(Int, Int)] = {
        def primeFactorsMultHelper(v: List[Int], acc: List[(Int, Int)]): List[(Int, Int)] = {

            v match {
                case Nil => Nil
                case x :: Nil => acc ::: List((x, 1))
                case x :: xs if (x == xs.head) && acc.isEmpty => {
                    primeFactorsMultHelper(xs, acc ::: List((x, 1)))
                }
                case x :: xs if (x == xs.head) => {
                    primeFactorsMultHelper(xs, acc ::: List((acc.head._1, acc.head._2 + 1)))
                }
                case x :: xs => {
                    primeFactorsMultHelper(xs, acc ::: List((x, 1)))
                }
            }
        }

        def accumulateResult(input: List[(Int, Int)], accResult: List[(Int, Int)]): List[(Int, Int)] = {
            input match {
                case Nil => Nil
                case x :: Nil => accResult ::: List((x._1, x._2))

                case x :: xs if (x._1 == xs.head._1) => {
                    accumulateResult(xs.tail, accResult ::: List((x._1, x._2 + 1)))
                }
                case x :: xs => accumulateResult(xs, accResult ::: List((x._1, x._2)))
            }
        }
        accumulateResult(primeFactorsMultHelper(primeFactors(input), Nil), Nil)
    }

    /*
     * Problem 39
     * (*) A list of prime numbers.
     * Given a range of integers by its lower and upper limit,
     * construct a list of all prime numbers in that range.
     * Example in Haskell:
     * P29> primesR 10 20
     * [11,13,17,19]
     */
    def primesR(lower: Int, upper: Int): List[Int] = {
        def primesRHelper(start: Int, acc: List[Int]): List[Int] = {
            start match {
                case x if start > upper => acc.reverse
                case x if isPrime(start) => primesRHelper(start + 1, x :: acc)
                case x => primesRHelper(start + 1, acc)
            }
        }
        primesRHelper(lower, Nil)
    }


    /* Problem 40
     * (**) Goldbach's conjecture.
     * Goldbach's conjecture says that every positive even number greater than 2
     * is the sum of two prime numbers. Example: 28 = 5 + 23.
     * It is one of the most famous facts in number theory that has not been proved to be
     * correct in the general case. It has been numerically confirmed up to very
     * large numbers (much larger than we can go with our Prolog system).
     * Write a predicate to find the two prime numbers that sum up to a given even integer.
     * Example:
     * (goldbach 28)
     * (5 23)
     */

    def goldbach(input: Int): (Int, Int) = {
        primesR(3, input) find { p => isPrime((input - p)) } match {
            case None => throw new IllegalArgumentException
            case Some(p1) => (p1, input - p1)
        }

    }

    /*
     * Problem 41
     * (**) Given a range of integers by its lower and upper
     * limit, print a list of all even numbers and their Goldbach composition.
     * In most cases, if an even number is written as the sum of two prime numbers,
     * one of them is very small. Very rarely, the primes are both bigger than say 50.
     * Try to find out how many such cases there are in the range 2..3000.
     */
    def goldbachList(lower: Int, upper: Int): List[(Int, Int)] = {
        (lower to upper filter (_ % 2 == 0)).toList map (p => goldbach(p))
    }


    def main(args: Array[String]): Unit = {
        try {
            isPrime(1)
        } catch {
            case e: IllegalArgumentException => true
            case e: Exception => println("exception caught: " + e);
        }
        assert(isPrime(7) == true)
        assert(isPrime(6) == false)
        assert(isPrime(2) == true)


        assert(gcd(36, 63) == 9)
        assert(gcd(35, 64) == 1)

        assert(coprime(35, 64) == true)

        assert(totientPhi(10) == 4)

        println(primeFactors(315))

        println(primeFactorsMult(315))
        println(primesR(10, 20))

        println(goldbach(28))

        println(goldbachList(9, 20))

    }
}
