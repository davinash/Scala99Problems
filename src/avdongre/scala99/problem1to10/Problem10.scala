package avdongre.scala99.problem1to10

/**
 * Created by adongre on 25/5/15.
 */

/**
 *
 * Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data
 * compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of
 * duplicates of the element E.
 * Example:
 * (encode '(a a a a b c c a a d e e e e))
 * ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
 *
 */
object Problem10 {

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

    def encode[T](list: List[T]): List[(Int, T)] = {
        def encodeHelper[T](packedList: List[Any], result: List[(Int, T)]): List[(Int, T)] = {
            packedList match {
                case Nil => Nil
                case (x: List[T]) :: Nil =>
                    result ::: List((x.length, x.head))
                case (x: List[T]) :: (xs: List[T]) =>
                    result ::: List((x.length, x.head)) ::: encodeHelper(xs, result)
                case _ => Nil
            }
        }
        encodeHelper(pack(list), Nil)
    }

    def main(args: Array[String]): Unit = {
        println(encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    }
}
