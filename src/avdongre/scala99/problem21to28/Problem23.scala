package avdongre.scala99.problem21to28

import scala.util.Random

/**
 * Created by adongre on 29/5/15.
 */


object Problem23 {

    def rndSelect[T](input: List[T], length: Int): List[T] = {
        val rg = new Random
        def split[T](input: List[T], length: Int): List[List[T]] = {
            def splitHelper[T](list: List[T], left: List[T], right: List[T], index: Int): List[List[T]] = {
                list match {
                    case Nil => List(left) ::: List(right)
                    case x :: xs => {
                        if (index <= length) {
                            splitHelper(xs, left ::: List(x), right, index + 1)
                        } else {
                            splitHelper(xs, left, right ::: List(x), index + 1)
                        }
                    }
                }
            }
            splitHelper(input, Nil, Nil, 1)
        }


        def rndSelectHelper(list: List[List[T]], result: List[T]): List[T] = {
            list match {
                case Nil => Nil
                case x :: xs :: Nil if result.length == length => result
                case x :: xs :: Nil => rndSelectHelper(
                    split(x ::: xs.tail, rg.nextInt(list.length - 1)), xs.head :: result)
            }
        }
        rndSelectHelper(split(input, rg.nextInt(input.length)), Nil)
    }

    def main(args: Array[String]) {
        println((rndSelect(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 3)))
        println((rndSelect(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 4)))
    }
}
