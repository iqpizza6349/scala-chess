package org.iqpizza

import PieceColor.{BLACK, WHITE}

import java.util.Scanner
import scala.util.control.Breaks

@main
def main(): Unit = {
  val board = Board()
  println(board)
  continueGame(board)
}

def letterForX(letter: Char) = letter.toUpper match {
  case 'A' => 0
  case 'B' => 1
  case 'C' => 2
  case 'D' => 3
  case 'E' => 4
  case 'F' => 5
  case 'G' => 6
  case 'H' => 7
  case _ => throw UnsupportedOperationException("Invalid letter")
}

def getUserMove: Move =
  val scanner = new Scanner(System.in)
  print("Your Move(e.g. A2 A4): ")
  var userMove = scanner.nextLine()
  userMove = userMove.replaceAll("\\s", "")

  val xFrom = letterForX(userMove.charAt(0))
  val yFrom = game.BOARD_HEIGHT - userMove.charAt(1).asDigit
  val xTo = letterForX(userMove.charAt(2))
  val yTo = game.BOARD_HEIGHT - userMove.charAt(3).asDigit
  Move(xFrom, yFrom, xTo, yTo)

def getValidUserMove(board: Board): Move =
  val loop = new Breaks
  var move: Move = null
  loop.breakable {
    while (true) {
      move = getUserMove
      var valid = false
      val possibleMoves = board.getPossibleMoves(WHITE)
      if possibleMoves.isEmpty then loop.break()
      for (possibleMove <- possibleMoves)
        if move == possibleMove then
          valid = true
          loop.break()
      if valid then
        loop.break()
      else println("Invalid Move")
    }
  }
  move

def continueGame(board: Board): Unit = {
  val loop = new Breaks
  loop.breakable {
    while (true) {
      val move = getValidUserMove(board)
      if move == null then
        if board.isCheck(WHITE) then
          println("Checkmate. Black Wins")
          loop.break()
        else
          println("Stalemate.")
          loop.break()

      board.performMove(move)
      println("Player move: " + move)
      println(board)

      val aiMove = AI.getBestMove(board, List[Move]())
      if aiMove == null then
        if board.isCheck(BLACK) then
          println("Checkmate. White Wins")
          loop.break()
        else
          println("Stalemate.")
          loop.break()

      board.performMove(aiMove)
      println("AI move: " + aiMove)
      println(board)
    }
  }
}
