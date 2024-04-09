import Types.{Board, Coord2D}

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val tasks = new Tasks
    val random = new MyRandom(scala.util.Random.nextLong())
    val (char, _) = tasks.randomChar(random)
    println(s"Generated character: $char")
    val board: Board = List(List.fill(4)(' '), List.fill(4)(' '), List.fill(4)(' '), List.fill(4)(' '))
    println(s"Starting Board: $board")
    val (words, positions) = tasks.readWordsWithPositions("teste.txt")
    println(s"Words: $words, Positions: $positions")

    val changes = tasks.setBoardWithWords(board, words, positions)
    println(s"New Board: $changes")



  }
}