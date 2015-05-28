/**
 * Created by adongre on 28/5/15.
 */
object Problem18 {
    def slice[T](input: List[T], left: Int, right: Int): List[T] = {
        def sliceHelper(list: List[T], result: List[T], index: Int): List[T] = {
            list match {
                case Nil => result
                case x :: xs => {
                    if (index >= left && index <= right)
                        sliceHelper(xs, result ::: List(x), index + 1)
                    else
                        sliceHelper(xs, result, index + 1)
                }
            }
        }
        sliceHelper(input, Nil, 1)
    }

    def main(args: Array[String]) {
        println(slice (List('a','b','c','d','e','f','g','h','i','k'),3, 7))
    }
}
