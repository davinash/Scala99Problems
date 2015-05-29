/**
 * Created by adongre on 29/5/15.
 */
object Problem22 {
    def range(start: Int, end: Int): List[Int] = {
        def rangeHelper(result: List[Int], index: Int): List[Int] = {
            if (index <= end)
                rangeHelper(result ::: List(index), index + 1)
            else
                result
        }
        rangeHelper(List(), start)
    }

    def main(args: Array[String]) {
        println(range(1, 4))
    }
}
