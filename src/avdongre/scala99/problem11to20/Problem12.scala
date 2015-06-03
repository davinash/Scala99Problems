package avdongre.scala99.problem11to20

/**
 * Created by adongre on 28/5/15.
 */
object Problem12 {
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
                    if (x.length == 1)
                        result ::: x
                    else
                        result ::: List((x.length, x.head))
                case (x: List[T]) :: (xs: List[T]) => {
                    if (x.length == 1)
                        result ::: x ::: encodeHelper(xs, result)
                    else
                        result ::: List((x.length, x.head)) ::: encodeHelper(xs, result)
                }
                case _ => Nil
            }
        }
        encodeHelper(pack(list), Nil)
    }

    def decodeModified[T](list: List[Any]): List[T] = {
        def decodeModifiedHelper[T](lst: List[Any], result: List[T]): List[T] = {
            lst match {
                case Nil => result
                case (x: (Int, T)) :: (xs: List[Any]) => {
                    decodeModifiedHelper(xs, result ::: List.fill(x._1)(x._2))
                }
                case (x: T) :: (xs: List[Any]) => {
                    decodeModifiedHelper(xs, result ::: List.fill(1)(x))
                }
                case _ => Nil
            }
        }
        decodeModifiedHelper(list, Nil)
    }

    def main(args: Array[String]) {
        val encodedList = encode_modified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
        println("Encoded List => " + encodedList)
        val x = decodeModified(encodedList)
        println(x)
    }
}
