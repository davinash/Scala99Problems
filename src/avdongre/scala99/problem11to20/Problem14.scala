package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem14 {
    def dupli[T](list: List[T]): List[T] = {
        def dupliHelper[T](lst: List[T], result: List[T]): List[T] = {
            lst match {
                case Nil => result
                case x :: xs => dupliHelper(xs, result ::: List(x, x) )
            }
        }

        dupliHelper(list, Nil)
    }

    def main(args: Array[String]) {
        println(dupli(List('a', 'b', 'c', 'c', 'd')))
    }
}
