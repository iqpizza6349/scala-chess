package org.iqpizza

import Type.{BISHOP, KING, KNIGHT, PAWN, QUEEN, ROOK}

import PieceColor.{BLACK, WHITE}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

enum Type(val shortcut: String, val worth: Int) {
    case PAWN extends Type("P", 100)
    case KING extends Type("K", 20000)
    case QUEEN extends Type("Q", 900)
    case BISHOP extends Type("B", 330)
    case KNIGHT extends Type("N", 320)
    case ROOK extends Type("R", 500)
}

abstract class Piece(var x: Int, var y: Int, val color: PieceColor,
                     val pieceType: Type, val worth: Int) extends Cloneable {

    def getPossibleDiagonalMoves(board: Board): List[Move] =
        val moves = ListBuffer[Move]()
        val loop = new Breaks

        loop.breakable {
            for (i <- 1 until 8)
                if !board.inBounds(x + i, y + i) then
                    loop.break
                val piece = board.getPiece(x + i, y + i)
                moves += getMove(board, x + i, y + i)
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                if !board.inBounds(x + i, y - i) then
                    loop.break()
                val piece = board.getPiece(x + i, y - i)
                moves += getMove(board, x + i, y - i)
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                if !board.inBounds(x - i, y - i) then
                    loop.break()
                val piece = board.getPiece(x - i, y - i)
                moves += getMove(board, x - i, y - i)
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                if !board.inBounds(x - i, y + i) then
                    loop.break()
                val piece = board.getPiece(x - i, y + i)
                moves += getMove(board, x - i, y + i)
                if piece != null then
                    loop.break()
        }

        removeNullFromMoves(moves.toList)

    def getPossibleHorizontalMoves(board: Board): List[Move] =
        val moves = List()
        val loop = new Breaks

        loop.breakable {
            for (i <- 1 until 8 - x)
                val piece = board.getPiece(x + i, y)
                moves.appended(getMove(board, x + i, y))
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                val piece = board.getPiece(x - i, y)
                moves.appended(getMove(board, x - i, y))
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                val piece = board.getPiece(x, y + i)
                moves.appended(getMove(board, x, y + i))
                if piece != null then
                    loop.break()
        }

        loop.breakable {
            for (i <- 1 until 8)
                val piece = board.getPiece(x, y - i)
                moves.appended(getMove(board, x, y - i))
                if piece != null then
                    loop.break()
        }

        removeNullFromMoves(moves)

    def getMove(board: Board, xTo: Int, yTo: Int): Move =
        var move: Move = null
        if board.inBounds(xTo, yTo) then
            val piece = board.getPiece(xTo, yTo)
            if piece != null then
                if piece.color != this.color then
                    move = Move(this.x, this.y, xTo, yTo)
            else
                move = Move(this.x, this.y, xTo, yTo)
        move;

    def removeNullFromMoves(list: List[Move]): List[Move] =
        var moves = List[Move]()
        for (mv <- list)
            if mv != null then
                moves = moves.appended(mv)
        moves

    override def clone(): AnyRef = super.clone()

    def getPossibleMoves(board: Board): List[Move]
}
