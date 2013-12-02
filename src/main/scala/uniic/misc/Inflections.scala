package uniic.misc

object Inflections {
  private val pluralSpecialCases = Map(
    "child" -> "children"
  )

  def plural(count: Int, word: String): String = {
    count + " " + pluralWord(word)
  }

  def pluralWord(count: Int, word: String): String = {
    if (count == 1) {
      word
    } else {
      pluralWord(word)
    }
  }

  def pluralWord(word: String): String = {
    pluralSpecialCases.get(word) match {
      case Some(specialCase) => specialCase
      case None => {
        if (word.endsWith("s")) {
          word + "es"
        } else {
          word + "s"
        }
      }
    }
  }

  def ordinal(n: Long) = {
    val suffix = {
      val tens = (n % 100) / 10
      if (tens == 1) { // eleventh, twelveth, ...
        "th"
      } else {
        (n % 10) match {
          case 1 => "st"
          case 2 => "nd"
          case 3 => "rd"
          case _ => "th"
        }
      }
    }
    n + suffix
  }
}
