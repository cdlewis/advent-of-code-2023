package Day3

import scala.io.Source
import scala.util.Using
import AOC.Reader

object Day3 {
    private val directions = Seq((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)).map(Point.apply)

    case class Point(x: Int, y: Int) {
        def +(anotherPoint: Point): Point = this.copy(x = this.x + anotherPoint.x, y = this.y + anotherPoint.y)
    }

    def solve(): Int = {
        val matrix = Reader
            .Read("day3_test")
            .split("\n")
            .map(_.toSeq)

        // Create a map of Point -> (Part Number, Number Start Point)
        val enginePartMap = matrix
            .zipWithIndex
            .map { (line, y) =>
                line
                    .toSeq
                    .zipWithIndex
                    .foldLeft((Seq[Seq[(Char, Point)]](), Seq[(Char, Point)]())) { case ((results, partialResult), (current, x)) =>
                        if (current.isDigit) (results, partialResult :+ (current, Point(x, y)))
                        else if (partialResult.nonEmpty) (results :+ partialResult, Seq[(Char, Point)]())
                        else (results, partialResult)
                    }
            }
            .flatMap { (results, remainder) =>
                if (remainder.nonEmpty) results :+ remainder
                else results
            }
            .map { partialResult =>
                    val number = partialResult.map(_._1).mkString.toInt
                    val root = partialResult(0)(1)
                    partialResult.map((_, point) => point -> (root, number))
            }
            .flatten
            .toMap

        val gears = matrix
            .zipWithIndex
            .map { (line, y) =>
                line.zipWithIndex.map { (char, x) =>
                    if (char == '*') {
                        Some(Point(x, y))
                    } else {
                        None
                    }
                }
            }
            .flatten

        gears
            .flatten.map { gearPosition =>
                directions
                    .map(direction => enginePartMap.get(gearPosition + direction))
                    .flatten
                    .toSet
            }
            .map { nearbyGears =>
                if (nearbyGears.size == 2) nearbyGears.map(_._2).product
                else 0
            }
            .sum
    }
}
