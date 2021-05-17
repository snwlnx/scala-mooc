package homeworks.futures

import scala.concurrent.{ExecutionContext, Future}

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
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    fullSequence2(futures).map(_.partitionMap(_.swap))


  def fullSequence2[A](futures: List[Future[A]])
                      (implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] =
    futures
      .map(toEither)
      .foldLeft(Future(List.empty[Either[Throwable, A]]))(addToList)
      .map(_.reverse)


  def toEither[A](future: Future[A])(implicit ex: ExecutionContext): Future[Either[Throwable, A]] =
    future.map(Right(_)).recover(Left(_))

  def addToList[A](listF: Future[List[Either[Throwable, A]]], future: Future[Either[Throwable, A]])
                  (implicit ex: ExecutionContext): Future[List[Either[Throwable, A]]] =
    listF.zipWith(future)((list, result) => result :: list)
}
