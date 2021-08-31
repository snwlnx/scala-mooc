package homeworks.futures


import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

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
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val wrapped = Future.sequence(
      futures
        .map(f => f.map(Right(_)))
        .map(_.recoverWith{
          case NonFatal(e) => Future.successful(Left(e))
        })
    )
    val succ = wrapped.map(l => l.collect{ case Right(v) => v})
    val err = wrapped.map(l => l.collect{ case Left(e) => e})
    succ.zip(err)
  }
}
