package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {

    @tailrec
    def step(l: List[Int], acc: List[Int] = Nil, cnt: Int = 0, num: Int = 0): List[Int] = l match {
      case Nil => acc ++ (cnt :: num :: Nil)
      case head::tail if num == 0 => step(tail, acc, cnt+1, head)
      case head::tail if num == head => step(tail, acc, cnt+1, head)
      case head::tail if num != head => step(tail, acc ++ (cnt :: num :: Nil) , 1, head)
    }

    step(currentLine)
    //task"Реализуйте функцию генерирующую след последовательность из текущей"()
  }


  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = {
    List[Int](1 ) #:: funSeq.map(nextLine(_))
    //task"Реализуйте ленивый список, который генерирует данную последовательность"()
  }
}