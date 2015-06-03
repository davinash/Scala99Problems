package avdongre.scala99.problem1to10

/**
 * Created by adongre on 25/5/15.
 */
object Problem8 {
    def compress[T](list: List[T]): List[T] = list match {
        case Nil => Nil
        case x :: xs => x :: compress(xs.dropWhile(_ == x))

    }

    def main(args: Array[String]): Unit = {
        val a = List('a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'c', 'c')
        println("Result ----> " + compress(a))
        val b = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println("Result ----> " + compress(b))
        println("Result ----> " + compress('a' to 'z' toList))

    }
}
