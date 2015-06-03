package avdongre.scala99.problem1to10

/**
 * Created by adongre on 25/5/15.
 */


object Problem1 {
    def myLast[T](list: List[T]): Option[T] = list match {
        case Nil => None
        case x :: Nil => {
            println("X =>" + x)
            Some(x)
        }
        case x :: xs => {
            println("X1 =>" + x)
            println("XS1 =>" + xs)
            myLast(xs)
        }
    }

    def main(args: Array[String]) {
        assert(myLast(1 to 10 toList) == Some(10))
        assert(myLast(List(1)) == Some(1))
        assert(myLast(Nil) == None)

        assert(myLast('a' to 'z' toList) == Some('z'))

    }
}

