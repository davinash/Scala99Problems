package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem15 {
    def repli[T](list: List[T], r: Int): List[T] = {
        def repliHelper[T](lst: List[T], result: List[T]): List[T] = {
            lst match {
                case Nil => result
                case x :: xs => repliHelper(xs, result ::: List.fill(r)(x))
            }
        }
        repliHelper(list, Nil)
    }

    def main(args: Array[String]) {
        println(repli(List('a', 'b', 'c'), 3))
    }

}
