/**
 * Created by adongre on 25/5/15.
 */


/**
 *
 * Problem 4
 * (*) Find the number of elements of a list.
 * Example in Haskell:
 * Prelude> myLength [123, 456, 789]
 * 3
 * Prelude> myLength "Hello, world!"
 * 13
 */

object Problem4 {
    def myLength[T](list: List[T]): Int = list match {
        case Nil => 0
        case x :: xs => 1 + myLength(xs)
    }

    def main(args: Array[String]): Unit = {
        assert(myLength(1 to 10 toList) == 10)
        assert(myLength('a' to 'z' toList) == 26)
        assert(myLength("Hello, world!" toList) == 13)
    }
}
