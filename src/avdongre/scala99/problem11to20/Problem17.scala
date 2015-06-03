package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem17 {
    def split[T](input: List[T], length: Int): List[List[T]] = {
        def splitHelper[T](list: List[T], left: List[T], right: List[T], index: Int): List[List[T]] = {
            list match
            {
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

    def main(args: Array[String]) {
        println(split(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'), 3))
    }

}
