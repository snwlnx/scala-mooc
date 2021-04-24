package homeworks.collections

object task_caesar {

  private val ALPHABET: IndexedSeq[Char] = 'A' to 'Z'

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
  def encrypt(word: String, offset: Int): String = word.map(encryptChar(offset))

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = encrypt(cipher, -offset)

  private def encryptChar(offset: Int)(char: Char): Char = {
    val index = calculateIndex(char, offset)
    if (index >= 0) {
      ALPHABET(index)
    } else {
      ALPHABET(index + ALPHABET.size)
    }
  }

  private def calculateIndex(char: Char, offset: Int) = (ALPHABET.indexOf(char) + offset) % ALPHABET.size
}
