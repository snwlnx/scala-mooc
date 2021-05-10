package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  val firstASCII = 65
  val lastASCII = 90

  def encrypt(word: String, offset: Int): String = {

    def transform(c: Char): Char = {
      val shift = c.toInt + offset
      shift match {
        case shift if(shift > lastASCII) => (shift - lastASCII + firstASCII - 1).toChar
        case shift if(shift < firstASCII) => (lastASCII - (firstASCII - shift) + 1).toChar
        case shift => shift.toChar
      }
    }

    def impl(input: String): String = {
      @tailrec
      def loop(input: String, acc: String = ""): String = input match {
        case "" => acc
        case input => loop(input.substring(1), acc + transform(input(0)))
      }
      loop(input)
    }

    impl(word)
    //task"Реализуйте метод `encrypt`"()
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    encrypt(cipher, -1 * offset)
    //task"Реализуйте метод `decrypt`"()
  }


}
