package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem19 {
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

    def rotate[T](input: List[T], at: Int): List[T] = {
        val r: List[List[T]] = if (at >= 0) split(input, at)
        else split(input, input.length + at)
        r match {
            case Nil => Nil
            /*
             * I got confuse here with pattern matching see here for my confusion
             * http://stackoverflow.com/questions/30523159/pattern-matching-for-list-of-list-in-scala
             */
            case x :: xs :: Nil => {
                xs ::: x
            }
        }
    }

    def main(args: Array[String]): Unit = {
        println(rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 3))
        println(rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), -2))
        println(rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 0))
        println(rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 100))
    }

}

