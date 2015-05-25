/**
 * Created by adongre on 25/5/15.
 */

/**
 * Problem 2
 * Find the last but one element of a list.
 * (Note that the Lisp transcription of this problem is incorrect.)
 * Example in Haskell:
 * Prelude> myButLast [1,2,3,4]
 * 3
 * Prelude> myButLast ['a'..'z']
 * 'y'
 */

object Problem2 {
    def myButLast[T](list: List[T]): Option[T] = list match {
        case Nil => None
        case x :: Nil => Some(x)
        case x :: xs => {
            if (xs.length == 1) Some(x)
            else
                myButLast(xs)
        }
    }

    def main(args: Array[String]): Unit = {
        assert(myButLast(1 to 10 toList) == Some(9))
        assert(myButLast(List(1)) == Some(1))
        assert(myButLast(Nil) == None)

        assert(myButLast('a' to 'z' toList) == Some('y'))
    }
}
