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

    }
}
