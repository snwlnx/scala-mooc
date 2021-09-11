package homeworks.futures

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
    * В данном задании Вам предлагается реализовать функцию fullSequence,
    * похожую на Future.sequence, но в отличии от нее,
    * возвращающую все успешные и не успешные результаты.
    * Возвращаемое тип функции - кортеж из двух списков,
    * в левом хранятся результаты успешных выполнений,
    * в правово результаты неуспешных выполнений.
    * Не допускается использование методов объекта Await и мутабельных переменных var
    */
  /**
    * @param futures список асинхронных задач
    * @return асинхронную задачу с кортежом из двух списков
    */
  def fullSequence[A](
      futures: List[Future[A]]
  )(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future.sequence(
      futures
        .map(_.transform(Success(_)))
    ) map (listTry =>
      listTry.foldLeft(Tuple2(List[A](), List[Throwable]()))((acc, tryResult) =>
        (acc, tryResult) match {
          case (_, Success(v))  => (acc._1 :+ v, acc._2)
          case (_, Failure(ex)) => (acc._1, acc._2 :+ ex)
        }
      )
    )
  }
}
