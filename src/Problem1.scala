/**
 * Created by adongre on 25/5/15.
 */


/**
 * Problem 1
 * Find the last element of a list.
 * (Note that the Lisp transcription of this problem is incorrect.)
 * Example in Haskell:*
 * Prelude> myLast [1,2,3,4]
 * 4
 * Prelude> myLast ['x','y','z']
 * 'z'
 * Source : https://wiki.haskell.org/99_questions/1_to_10
 */

object Problem1 {
    def myLast[T](list: List[T]): Option[T] = list match {
        case Nil => None
        case x :: Nil => Some(x)
        case x :: xs => myLast(xs)
    }

    def main(args: Array[String]) {
        assert(myLast(1 to 10 toList) == Some(10))
        assert(myLast(List(1)) == Some(1))
        assert(myLast(Nil) == None)

        assert(myLast('a' to 'z' toList) == Some('z'))

    }
}

