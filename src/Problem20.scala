/**
 * Created by adongre on 29/5/15.
 */
object Problem20 {
    def split[T](input: List[T], length: Int): List[List[T]] = {
        def splitHelper[T](list: List[T], left: List[T], right: List[T], index: Int): List[List[T]] = {
            list match {
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

    def remove_at[T](at:Int, input:List[T]) : List[T] = {
        val r = split(input, at - 1)
        r match  {
            case Nil => Nil
            case x::xs::Nil => x ::: xs.tail
        }
    }

    def main(args: Array[String]) {
        println(remove_at(2, List('a','b','c','d')))
    }
}
