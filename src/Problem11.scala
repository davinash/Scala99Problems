/**
 * Created by adongre on 28/5/15.
 */
object Problem11 {
    def pack(list: List[Any]): List[Any] = {
        if (list.isEmpty) List(List())
        else {
            val (packed, next) = list span {
                _ == list.head
            }
            if (next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def encode_modified[T](list: List[T]): List[Any] = {
        def encodeHelper[T](packedList: List[Any], result: List[(Int, T)]): List[Any] = {
            packedList match {
                case Nil => Nil
                case (x: List[T]) :: Nil =>
                    if ( x.length == 1)
                        result ::: x
                        else
                    result ::: List((x.length, x.head))
                case (x: List[T]) :: (xs: List[T]) => {
                    if ( x.length == 1)
                        result ::: x ::: encodeHelper(xs, result)
                        else
                    result ::: List((x.length, x.head)) ::: encodeHelper(xs, result)
                }
                case _ => Nil
            }
        }
        encodeHelper(pack(list), Nil)
    }

    def main(args: Array[String]) {
        println(encode_modified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
    }
}
