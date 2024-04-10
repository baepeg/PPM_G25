import Types.Direction.Direction
import Types.{Board, Coord2D, Direction}

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

  //T4- Preencher os espaços vazios do tabuleiro aleatoriamente(não devia retornar o board só?)
//  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom)

  // T5: Verificar se a palavra existe no tabuleiro a partir de uma posição e direção inicial
  def play(board: Board, word: String, startCoord: Coord2D, direction: Direction): Boolean = {
    val (row, col) = startCoord
    val wordLength = word.length

    def checkWord(currentRow: Int, currentCol: Int, charIndex: Int): Boolean = {
      if (charIndex == wordLength) true
      else if (currentRow < 0 || currentRow >= board.length || currentCol < 0 || currentCol >= board(currentRow).length) false
      else if (board(currentRow)(currentCol) != word(charIndex)) false
      else {
        val (dRow, dCol) = direction match {
          case Direction.North => (-1, 0)
          case Direction.South => (1, 0)
          case Direction.East => (0, 1)
          case Direction.West => (0, -1)
          case Direction.NorthEast => (-1, 1)
          case Direction.NorthWest => (-1, -1)
          case Direction.SouthEast => (1, 1)
          case Direction.SouthWest => (1, -1)
        }
        checkWord(currentRow + dRow, currentCol + dCol, charIndex + 1)
      }
    }

    checkWord(row, col, 0)
  }

  def play2(board: Board, word: String, startCoord: Coord2D): Boolean = {
    val wordLength = word.length

    // Função auxiliar para verificar a palavra a partir de uma posição e direção
    def checkWordFromPosition(currentCoord: Coord2D, charIndex: Int, visited: Set[Coord2D]): Boolean = {
      if (charIndex == wordLength) true // Verificamos toda a palavra
      else if (!board.indices.contains(currentCoord._1) || !board(currentCoord._1).indices.contains(currentCoord._2)) false // Coordenada fora do tabuleiro
      else if (visited.contains(currentCoord)) false // Coordenada já visitada
      else if (board(currentCoord._1)(currentCoord._2) != word(charIndex)) false // Letra diferente
      else {
        // Verifica todas as posições adjacentes
        val adjacentCoords = List(
          (currentCoord._1 - 1, currentCoord._2), // Norte
          (currentCoord._1 + 1, currentCoord._2), // Sul
          (currentCoord._1, currentCoord._2 - 1), // Oeste
          (currentCoord._1, currentCoord._2 + 1), // Leste
          (currentCoord._1 - 1, currentCoord._2 - 1), // Noroeste
          (currentCoord._1 - 1, currentCoord._2 + 1), // Nordeste
          (currentCoord._1 + 1, currentCoord._2 - 1), // Sudoeste
          (currentCoord._1 + 1, currentCoord._2 + 1) // Sudeste
        )

        // Verifica a palavra para cada posição adjacente
        adjacentCoords.exists(adjCoord => checkWordFromPosition(adjCoord, charIndex + 1, visited + currentCoord))
      }
    }

    // Inicia a verificação a partir da posição inicial
    checkWordFromPosition(startCoord, 0, Set.empty)
  }


}
