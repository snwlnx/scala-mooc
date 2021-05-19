package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.collection.immutable.NumericRange

object task_caesar {

  val alphabet: NumericRange.Inclusive[Char] = 'A' to 'Z'

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
    word.map(c => {
      val alphabetIndex = alphabet.indexOf(c);
      alphabet((alphabetIndex + offset) % alphabet.size)
    })

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher.map(c => {
      val cipherIndex = alphabet.indexOf(c)
      alphabet((alphabet.size + (cipherIndex - offset % alphabet.size)) % alphabet.size)
    })

}
