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

    override def toString: String = color.shortcut + pieceType.shortcut + " "

    override def clone(): AnyRef = throw UnsupportedOperationException("Abstract class cannot clone itself!")

    def getPossibleMoves(board: Board): List[Move]
}

class Rook(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, ROOK, ROOK.worth) {
    override def getPossibleMoves(board: Board): List[Move] = getPossibleHorizontalMoves(board)

    override def clone(): AnyRef = new Rook(x, y, color)
}

class Knight(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, KNIGHT, KNIGHT.worth) {
    override def getPossibleMoves(board: Board): List[Move] =
        val moves = ListBuffer[Move]()
        moves += getMove(board, x + 2, y + 1)
        moves += getMove(board, x - 1, y + 2)
        moves += getMove(board, x - 2, y + 1)
        moves += getMove(board, x + 1, y - 2)
        moves += getMove(board, x + 2, y - 1)
        moves += getMove(board, x + 1, y + 2)
        moves += getMove(board, x - 2, y - 1)
        moves += getMove(board, x - 1, y - 2)
        removeNullFromMoves(moves.toList)

    override def clone(): AnyRef = new Knight(x, y, color)
}

class Bishop(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, BISHOP, BISHOP.worth) {
    override def getPossibleMoves(board: Board): List[Move] =
        getPossibleDiagonalMoves(board)

    override def clone(): AnyRef = new Bishop(x, y, color)
}

class Queen(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, QUEEN, QUEEN.worth) {
    override def getPossibleMoves(board: Board): List[Move] =
        val diagonal = getPossibleDiagonalMoves(board)
        val horizontal = getPossibleHorizontalMoves(board)
        horizontal.appendedAll(diagonal)

    override def clone(): AnyRef = new Queen(x, y, color)
}

class King(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, KING, KING.worth) {
    override def getPossibleMoves(board: Board): List[Move] =
        val moves = ListBuffer[Move]()
        moves += getMove(board, x + 1, y)
        moves += getMove(board, x + 1, y + 1)
        moves += getMove(board, x, y + 1)
        moves += getMove(board, x - 1, y + 1)
        moves += getMove(board, x - 1, y)
        moves += getMove(board, x - 1, y - 1)
        moves += getMove(board, x, y - 1)
        moves += getMove(board, x + 1, y - 1)
        moves += getCastleKingSideMove(board)
        moves += getCastleQueenSideMove(board)
        removeNullFromMoves(moves.toList)

    def getCastleKingSideMove(board: Board): Move =
        val pieceInCorner = board.getPiece(x + 3, y)
        if !canCastle(pieceInCorner, board) then return null
        if board.getPiece(x + 1, y) != null || board.getPiece(x + 2, y) != null then
            return null
        new Move(x, y, x + 2, y)

    def getCastleQueenSideMove(board: Board): Move =
        val pieceInCorner = board.getPiece(x - 4, y)
        if !canCastle(pieceInCorner, board) then return null
        if board.getPiece(x + 1, y) != null || board.getPiece(x - 2, y) != null || board.getPiece(x - 3, y) != null then
            return null
        new Move(x, y, x - 2, y)

    private def canCastle(piece: Piece, board: Board): Boolean =
        if piece == null || piece.pieceType != ROOK then return false
        if piece.color != color then return false
        if color == WHITE && board.whiteKingMoved then return false
        if color == BLACK && board.blackKingMoved then return false
        true

    override def clone(): AnyRef = new King(x, y, color)
}

class Pawn(x: Int, y: Int, color: PieceColor) extends Piece(x, y, color, PAWN, PAWN.worth) {
    override def getPossibleMoves(board: Board): List[Move] =
        val moves = ListBuffer[Move]()
        var direction = -1
        if color == BLACK then direction = 1

        if board.getPiece(x, y + direction) == null then
            moves += getMove(board, x, y + direction)
        if isStartingPosition() && board.getPiece(x, y + direction) == null
            && board.getPiece(x, y + direction * 2) == null then
            moves += getMove(board, x, y + direction * 2)

        var piece = board.getPiece(x + 1, y + direction)
        if piece != null then moves += getMove(board, x + 1, y + direction)
        piece = board.getPiece(x - 1, y + direction)
        if piece != null then moves += getMove(board, x - 1, y + direction)

        removeNullFromMoves(moves.toList)

    def isStartingPosition(): Boolean =
        if color == BLACK then y == 1
        else y == 8 - 2

    override def clone(): AnyRef = new Pawn(x, y, color)
}
