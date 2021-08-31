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
    //encryptWithCollections(word, offset)
    encryptSimple(word, offset)

  def encryptSimple(word: String, offset: Int): String = {
    val total = 'Z' - 'A' + 1
    val off = if (offset < 0) 'Z' - Math.abs(offset) % total - 'A' + 1 else offset % total
    word
      .map{ ch => (Math.abs(ch - 'A' + off) % total + 'A').toChar
    }
  }

  def encryptWithCollections(word: String, offset: Int) = {
    val total = 'Z' - 'A' + 1
    val off = if (offset < 0) 'Z' - Math.abs(offset) % total - 'A' + 1 else offset % total
    val translation = ('A' to 'Z')
      .map(ch => ch -> (Math.abs(ch - 'A' + off) % total + 'A').toChar)
      .toMap
    word
      .flatMap(ch => translation.get(ch))
      .mkString
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = 
    //encryptWithCollections(cipher, -offset)
    encryptSimple(cipher, -offset)

}
