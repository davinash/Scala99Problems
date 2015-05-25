/**
 * Created by adongre on 25/5/15.
 */

/**
 *
 * Problem 3
 * Find the K'th element of a list. The first element in the list is number 1.
 * Example:
 * (element-at '(a b c d e) 3)
 * c
 * Example in Haskell:
 * Prelude> elementAt [1,2,3] 2
 * 2
 * Prelude> elementAt "haskell" 5
 * 'e'
 */


object Problem3 {
    def elementAt[T](list: List[T], index: Int): Option[T] = {

        def loop(subList: List[T]): Option[T] = subList match {
            case Nil => None
            case x :: xs => {
                if (xs.length == list.length - index) Some(x)
                else loop(xs)
            }
        }

        if (index > list.length) None
        if (index < 0) None

        loop(list)
    }

    def main(args: Array[String]) {
        assert(elementAt(1 to 10 toList, 5) == Some(5))
        assert(elementAt('a' to 'z' toList, 10) == Some('j'))
        assert(elementAt(Nil, 10) == None)
        assert(elementAt('a' to 'z' toList, 100) == None)
        assert(elementAt('a' to 'z' toList, -1) == None)

    }
}
