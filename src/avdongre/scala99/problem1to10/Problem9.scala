package avdongre.scala99.problem1to10

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
        if (list.isEmpty) List(List())
        else {
            val (packed, next) = list span {
                _ == list.head
            }
            if (next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def main(args: Array[String]) {
        println(pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    }

}