package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem16 {
    def dropEvery[T](input: List[T], nth: Int): List[T] = {
        def dropHelper(list: List[T], result: List[T], index: Int): List[T] = {
            list match {
                case Nil => result
                case x :: xs => {
                    if (index == nth) {
                        dropHelper(xs, result, 1)
                    }
                    else {
                        dropHelper(xs, result ::: List(x), index + 1)
                    }
                }
            }

        }
        dropHelper(input, Nil, 1)
    }

    def main(args: Array[String]) {
        println(dropEvery(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'), 3))
    }
}
