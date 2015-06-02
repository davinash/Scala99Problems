import scala.util.Random

/**
 * Created by adongre on 2/6/15.
 */
object Problem25 {
    def split[T](input: List[T], length: Int): List[List[T]] = {
        //println ( input )
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

    def rndPermu[T](input: List[T]): List[T] = {
        val rg = new Random
        def rndPermuHelper(list: List[List[T]], result: List[T]): List[T] = {
            list match {
                case Nil => result
                case (left: List[T]) :: (right: List[T]) :: Nil if left.isEmpty && right.isEmpty => result
                case (left: List[T]) :: (right: List[T]) :: Nil =>

                    if (right.isEmpty) {
                        rndPermuHelper(split(left, rg.nextInt(input.length - 1)), result)
                    } else {
                        rndPermuHelper(split(left ::: right.tail, rg.nextInt(input.length - 1)), right.head :: result)
                    }
            }
        }
        rndPermuHelper(split(input, rg.nextInt(input.length)), Nil)
    }

    def main(args: Array[String]): Unit = {
        println(rndPermu(List('a', 'b', 'c', 'd', 'e', 'f')))
        println(rndPermu(List('a', 'b', 'c', 'd', 'e', 'f')))
        println(rndPermu(List('a', 'b', 'c', 'd', 'e', 'f')))
        println(rndPermu(List('a', 'b', 'c', 'd', 'e', 'f')))
        println(rndPermu(List('a', 'b', 'c', 'd', 'e', 'f')))

    }
}
