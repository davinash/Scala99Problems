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

    def encode[T](list: List[T]): List[(Int, T)] = {
        pack(list) map { p => ( p.length, p.head ) }
    }

    def main(args: Array[String]): Unit = {
        println(encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    }
}
