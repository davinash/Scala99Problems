/**
 * Created by adongre on 25/5/15.
 */
object Problem7 {
    def myflatten[T](list: List[T]): List[T] = list match {
        case Nil => list
        case (x: List[T]) :: xs => myflatten(x) ::: myflatten(xs)
        case x :: xs => x :: myflatten(xs)

    }

    def main(args: Array[String]) {
        println(myflatten(List('a', List('b', List('c', 'd'), 'e'))))
        println(myflatten(List('X', ('a' to 'z' toList))))
    }
}
