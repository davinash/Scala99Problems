import scala.compat.Platform

/**
 * Created by adongre on 25/5/15.
 */

/**
 * Problem 9
 * (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
 * Example:
 * (pack '(a a a a b c c a a d e e e e))
 * ((A A A A) (B) (C C) (A A) (D) (E E E E))
 */
object Problem9 {
    def pack(list: List[Any]): List[Any] = {
        def packHelper(input: List[Any], currentElement: Any, result: List[Any]): List[Any] = {
            input match {
                case Nil => result
                case x :: xs if x == currentElement => packHelper(xs, x, x :: result)
                case x :: xs if result == Nil => packHelper(xs, x, x :: Nil)
                case x :: xs => result :: packHelper(xs, x, x :: Nil)
            }
        }
        packHelper(list, Nil, Nil)
    }

    def main(args: Array[String]) {
        //pack(List(List('A', 'A', 'A', 'A'), List('B'), List('C', 'C'), List('A', 'A'), List('D'), List('E', 'E', 'E', 'E')))
        println(pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    }

}