package avdongre.scala99.problem1to10

/**
 * Created by adongre on 25/5/15.
 */

/**
 * 6 Problem 6
 * (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
 * Example in Haskell:
 * Main> isPalindrome [1,2,3]
 * False
 * Main> isPalindrome "madamimadam"
 * True
 * Main> isPalindrome [1,2,4,8,16,8,4,2,1]
 * True
 *
 */

object Problem6 {
    def isPalindrome[T](list : List[T]) : Boolean = list match {
        case Nil => true
        case x::Nil => true
        case x::xs => (list.head == list.last) && isPalindrome(list.tail.init)
    }
    def main(args: Array[String]) {
        assert ( isPalindrome(List(1,2,3)) == false)
        assert ( isPalindrome("madamimadam".toList) == true )
        assert ( isPalindrome(List(1,2,4,8,16,8,4,2,1)) == true )
    }
}
