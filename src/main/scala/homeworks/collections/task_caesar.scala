package homeworks.collections

object task_caesar {
  final val alphabetSize = 26
  final val maxAlphabetIndex = 90
  final val startAlphabetIndex = 65

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
  def encrypt(word: String, offset: Int): String = {
    val normalizedShift = offset % alphabetSize;

    val shiftedCharList: Array[Char] = word.toCharArray.map((char: Char) => {
      val newCode = char.hashCode + normalizedShift
      val charValue = if (newCode > maxAlphabetIndex) newCode - alphabetSize else newCode
      charValue.toChar
    })

    shiftedCharList.foldLeft("")((acc, curr) => acc + curr.toString)
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val normalizedShift = offset % alphabetSize;
    val shiftedCharList = cipher.toCharArray.map((char: Char) => {
      val newCode = char.hashCode - normalizedShift
      val charValue = if (newCode < startAlphabetIndex) newCode + alphabetSize else newCode
      charValue.toChar
    })

    shiftedCharList.foldLeft("")((acc, curr) => acc + curr.toString)
  }
}
