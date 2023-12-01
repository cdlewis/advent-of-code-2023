package Day1

import scala.io.Source
import scala.util.Using
import AOC.Reader

val digitChars = Array('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')

val digitMapping = Map[String, Int](
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
)

object Day1 {
    def Solve(): Int = {
        val findDigit: PartialFunction[String, Int] = {
            case token if token.nonEmpty && token(0).isDigit => token(0).asDigit
        }
        val findEnglishNumber: PartialFunction[String, Int] = ((token: String) =>
            digitMapping.find((digit, _) => token.startsWith(digit)).map(_._2)).unlift
        val findAnyNumber = findDigit.orElse(findEnglishNumber)

        Reader
            .Read("day1_test")
            .split("\n")
            .map { line =>
                val tokens = line.tails.toSeq
                val first = tokens.collectFirst(findAnyNumber).get
                val second = tokens.reverse.collectFirst(findAnyNumber).get
                10 * first + second
            }
            .sum
    }
}

