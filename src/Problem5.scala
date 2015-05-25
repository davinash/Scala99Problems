/**
 * Created by adongre on 25/5/15.
 */

/**
 *
 * Problem 5
 * (*) Reverse a list.
 * Example in Haskell:
 * Prelude> myReverse "A man, a plan, a canal, panama!"
 * "!amanap ,lanac a ,nalp a ,nam A"
 * Prelude> myReverse [1,2,3,4]
 * [4,3,2,1]
 */

object Problem5 {
    def myReverse[T](list: List[T]): List[T] = list match {
        case Nil => Nil
        case x::xs => myReverse(xs) ::: List(x)
    }

    def main(args: Array[String]): Unit = {
        assert( myReverse(1 to 10 toList) == (10 to 1 by -1 toList))
        assert( myReverse(List('a','b','c','d','e')) == (List('e','d','c','b','a')))
    }
}
