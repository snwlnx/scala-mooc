import homeworks.futures.task_futures_sequence

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val f1 = Future.failed(new Exception("ex1"))
val f2 = Future.successful(2);
val f3 = Future.successful((5));
val f4 = Future.failed(new Exception("ex4"))
val lst = List(f1, f2, f3, f4)

task_futures_sequence.fullSequence(lst)
Thread.sleep(100)