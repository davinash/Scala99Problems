/**
 * Created by adongre on 29/5/15.
 */
object Problem21 {
    def split[T](input: List[T], length: Int): List[List[T]] = {
        def splitHelper[T](list: List[T], left: List[T], right: List[T], index: Int): List[List[T]] = {
            list match
            {
                case Nil => List(left) ::: List(right)
                case x :: xs => {
                    if (index <= length) {
                        splitHelper(xs, left ::: List(x), right, index + 1)
                    } else {
                        splitHelper(xs, left, right ::: List(x), index + 1)
                    }
                }
            }
        }
        splitHelper(input, Nil, Nil, 1)
    }
    def insertAt[T](newElement:T, input:List[T], position:Int) : List[T] = {
        val r = split(input, position - 1)
        r match {
            case Nil => Nil
            case left::right::Nil => left ::: List(newElement) ::: right
            case _ => Nil
        }
    }
    def main(args: Array[String]) {
        println(insertAt('X', List('a','b','c','d'), 2))
    }
}
