package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle extends App {

  /**
   * 1 - единица
   * 1 1 - одна единица (см. выше)
   * 2 1 - две единицы (см. выше)
   * 1211 - одна двойка одна единица (см. выше)
   * 111221 - одна единица одна двойка две единицы (см. выше)
   * 312211 - три единицы две двойки одна единица (см. выше)
   * и т.д.
   */

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
    def nextLineRec(list: List[Int], acc: List[Int], charCounter: Int, prev: Int): List[Int] = {
      list match {
        case Nil => acc ++ (charCounter :: prev :: Nil)
        case cur :: _ => {
          val counter = if (cur == prev) charCounter + 1 else 1
          val newAcc: List[Int] = if (counter == 1 && cur != prev) acc ++ (charCounter :: prev :: Nil)  else acc

          nextLineRec(list.tail, newAcc, counter, cur)
        }
      }
    }

    nextLineRec(currentLine.tail, List.empty[Int], 1, currentLine.head)
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map{n => nextLine(n)}

  println(funSeq(5))
}
