package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

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

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine.foldLeft(Vector((0, currentLine.head)))(
      (cnt, el) => 
        if (el == cnt.last._2) 
          cnt.dropRight(1).appended((cnt.last._1 + 1, el))
        else
          cnt.appended((1, el))
      )
      .flatMap(el => List(el._1, el._2))
      .toList

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] =
    List(1) #:: List(1, 1) #:: funSeq.zip(funSeq.tail).map(n => nextLine(n._2))
}