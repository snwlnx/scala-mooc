package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

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
  def encrypt(word: String, offset: Int): String =
    word.map(c => (
      'A'.toInt +
        (
          c.toInt  - 'A'.toInt + offset % ('Z'.toInt - 'A'.toInt + 1))
          %
          ('Z'.toInt - 'A'.toInt + 1)
      ).toChar
    )

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher.map(c =>
      ('A'.toInt +
        (
          c.toInt
          - 'A'.toInt
          + (
              ('Z'.toInt - 'A'.toInt + 1)
              +
              (('Z'.toInt - 'A'.toInt + 1) - offset) % ('Z'.toInt - 'A'.toInt + 1)
            )
          %
          ('Z'.toInt - 'A'.toInt + 1)
        ) % ('Z'.toInt - 'A'.toInt + 1)
      ).toChar
    )
}
