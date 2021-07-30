package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  private val alp: IndexedSeq[Char] = 'A' to 'Z'
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
    word.map(ch => alp(getIndex(ch, offset)))



  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    encrypt(cipher, -offset)

  def getIndex(char: Char, offSet: Int) : Int = {
    val index = (alp.indexOf(char) + offSet) % alp.size
    if (index < 0)
      alp.size + index
    else
      index
  }

}
