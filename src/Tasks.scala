import Types.Board
import Types.Coord2D

import scala.io.Source

class Tasks() {

  //T1- Gerar um caractér aleatório (Não devia retornar só um char?)
  def randomChar(rand: MyRandom): (Char, MyRandom) =
  {
    val (randomInt, _) = rand.nextInt(26)
    val randomChar = ('A' + randomInt).toChar
    (randomChar, rand)
  }

  //T2- Preencher uma posicao do board
  def fillOneCell(board: Board, letter: Char, coord: Coord2D) : Board =
  {
    board.updated(coord._1, board(coord._1).updated(coord._2, letter))
  }

  //T3- Preencher o board baseado num ficheiro
  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

    def helper(b: Board, word: String, position: List[Coord2D]): Board = {
      if (word.isEmpty || position.isEmpty) b // Base case: return the current board
      else {
        val newBoard = fillOneCell(b, word.head, position.head) // Update the cell
        helper(newBoard, word.tail, position.tail) // Recursively call with the updated board
      }
    }

    words match {
      case Nil => board // Base case: No more words, return the board
      case head :: tail =>
        val updatedBoard = helper(board, head, positions.head) // Set the first word
        setBoardWithWords(updatedBoard, tail, positions.tail) // Recursively set remaining words (using updated board)
    }
  }


  //T3 Helper

  def readWordsWithPositions(filename: String): (List[String], List[List[Coord2D]]) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList.zipWithIndex // Combine lines with their line numbers (indexes)
    source.close()

    val (words, positions) = lines.unzip // Separate words and line numbers
    val upperCaseWords = words.map(_.toUpperCase) // Convert words to uppercase

    // Create a list of Coord2D lists, one for each word
    val coord2DPositions : List[List[Coord2D]] = List(positions.zip(words).flatMap {
      case (lineNumber, word) => word.map(char => (lineNumber, word.indexOf(char)))
    })

    (upperCaseWords, coord2DPositions)
  }


}
