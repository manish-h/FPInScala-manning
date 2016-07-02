package chap2

import scala.annotation.tailrec
import java.util.stream.IntStream
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

object NthFibonacci {

  // Ex 2.1
  def fib(n: Int): Int = {
    @tailrec
    def fibInternal(previous1: Int, previous2: Int, curr: Int): Int = {
      if (curr == n)
        previous1
      else
        fibInternal(previous1 + previous2, previous1, curr + 1)
    }
    if (n == 1 || n == 2)
      n - 1
    else
      fibInternal(1, 0, 2)
  }                                               //> fib: (n: Int)Int

  println(s"Fib n ${fib(1)}")                     //> Fib n 0
  
  Stream.iterate(1, 10)(_+1) foreach { x => println(fib(x)) }
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 5
                                                  //| 8
                                                  //| 13
                                                  //| 21
                                                  //| 34
}