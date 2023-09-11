package org.iqpizza

import PieceColor.{BLACK, WHITE}
import Type.{KING, PAWN}
import game.{BOARD_HEIGHT, BOARD_WIDTH}

import scala.util.boundary

abstract class Board extends Cloneable {

    def copy(): Board

    def getPiece(x: Int, y: Int): Piece

    def inBounds(x: Int, y: Int): Boolean

    def getPossibleMoves(color: PieceColor): List[Move]

    def performMove(move: Move): Unit

    def movePiece(piece: Piece, xTo: Int, yTo: Int): Unit

    def isCheck(color: PieceColor): Boolean

    def whiteKingMoved: Boolean

    def blackKingMoved: Boolean

    def pieces: Array[Array[Piece]]

    override def toString: String = super.toString
}

object Board {
    private class standardBoard(var chessPieces: Array[Array[Piece]],
                                var whiteKingMove: Boolean, var blackKingMove: Boolean) extends Board {


        override def copy(): Board = clone()

        override def clone(): Board =
            val chessPieces = Array.ofDim[Piece](BOARD_WIDTH, BOARD_HEIGHT)
            for (x <- 0 until BOARD_WIDTH)
                for (y <- 0 until BOARD_HEIGHT)
                    val piece = this.chessPieces(x)(y)
                    if piece != null then
                        chessPieces(x)(y) = piece.clone().asInstanceOf[Piece]
            new standardBoard(chessPieces, this.whiteKingMoved, this.blackKingMoved)

        override def getPiece(x: Int, y: Int): Piece =
            if !inBounds(x, y) then
                null
            else
                chessPieces(x)(y)

        override def inBounds(x: Int, y: Int): Boolean =
            (x >= 0 && y >= 0) && (x < BOARD_WIDTH && y < BOARD_HEIGHT)

        def getPossibleMoves(color: PieceColor): List[Move] =
            var moves = List[Move]()
            for (x <- 0 until BOARD_WIDTH)
                for (y <- 0 until BOARD_HEIGHT)
                    val piece = this.chessPieces(x)(y)
                    if piece != null then
                        if piece.color == color then
                            moves = moves.concat(piece.getPossibleMoves(this))
            moves

        def performMove(move: Move): Unit = {
            val piece = chessPieces(move.xFrom)(move.yFrom)
            if (piece == null) {
                return
            }
            movePiece(piece, move.xTo, move.yTo)

            if piece.pieceType == PAWN then
                if piece.y == 0 || piece.y == BOARD_HEIGHT - 1 then
                    chessPieces(piece.x)(piece.y) = Queen(piece.x, piece.y, piece.color)
            if piece.pieceType == KING then
                // mark the king as having moved.
                if piece.color == WHITE then
                    whiteKingMove = true
                else
                    blackKingMove = true

                if move.xTo - move.xFrom == 2 then
                    // check king side castling
                    val rook = chessPieces(piece.x + 1)(piece.y)
                    movePiece(rook, piece.x + 1, piece.y)
                if move.xTo - move.xFrom == -2 then
                    // check queen side castling
                    val rook = chessPieces(piece.x - 2)(piece.y)
                    movePiece(rook, piece.x + 1, piece.y)
        }

        def movePiece(piece: Piece, xTo: Int, yTo: Int): Unit = {
            chessPieces(piece.x)(piece.y) = null
            piece.x = xTo
            piece.y = yTo
            chessPieces(xTo)(yTo) = piece
        }

        def isCheck(color: PieceColor): Boolean =
            var otherColor: PieceColor = WHITE
            if color == WHITE then
                otherColor = BLACK

            boundary:
                for (mv <- getPossibleMoves(otherColor))
                    val copy = clone().asInstanceOf[standardBoard]
                    copy.performMove(mv)

                    var kingFound = false
                    for (x <- 0 until BOARD_WIDTH)
                        for (y <- 0 until BOARD_HEIGHT)
                            val piece = copy.chessPieces(x)(y)
                            if piece != null then
                                if piece.color == color && piece.pieceType == KING then
                                    kingFound = true

                    if !kingFound then
                        boundary.break(true)
                false

        override def whiteKingMoved: Boolean = this.whiteKingMove

        override def blackKingMoved: Boolean = this.blackKingMove

        override def pieces: Array[Array[Piece]] = this.chessPieces

        override def toString: String =
            var string = "    A  B  C  D  E  F  G  H\n"
            string += "    -----------------------\n"
            for (y <- 0 until BOARD_HEIGHT)
                string += (8 - y) + " | "
                for (x <- 0 until BOARD_WIDTH)
                    val piece = chessPieces(x)(y)
                    if piece != null then string += piece.toString
                    else string += ".. "
                string += "\n"
            string + "\n"
    }

    private def standardBoard: Array[Array[Piece]] =
        val chessPieces = Array.ofDim[Piece](BOARD_WIDTH, BOARD_HEIGHT)

        // pawns
        for (x <- 0 until BOARD_WIDTH)
            chessPieces(x)(BOARD_HEIGHT - 2) = Pawn(x, BOARD_HEIGHT - 2, WHITE)
            chessPieces(x)(1) = Pawn(x, 1, BLACK)

        // rooks
        chessPieces(0)(BOARD_HEIGHT - 1) = Rook(0, BOARD_HEIGHT - 1, WHITE)
        chessPieces(BOARD_WIDTH - 1)(BOARD_HEIGHT - 1) = Rook(BOARD_WIDTH - 1, BOARD_HEIGHT - 1, WHITE)
        chessPieces(0)(0) = Rook(0, 0, BLACK)
        chessPieces(BOARD_WIDTH - 1)(0) = Rook(BOARD_WIDTH - 1, 0, BLACK)

        // knights
        chessPieces(1)(BOARD_HEIGHT - 1) = Knight(1, BOARD_HEIGHT - 1, WHITE)
        chessPieces(BOARD_WIDTH - 2)(BOARD_HEIGHT - 1) = Knight(BOARD_WIDTH - 2, BOARD_HEIGHT - 1, WHITE)
        chessPieces(1)(0) = Knight(1, 0, BLACK)
        chessPieces(BOARD_WIDTH - 2)(0) = Knight(BOARD_WIDTH - 2, 0, BLACK)

        // bishops
        chessPieces(2)(BOARD_HEIGHT - 1) = Bishop(2, BOARD_HEIGHT - 1, WHITE)
        chessPieces(BOARD_WIDTH - 3)(BOARD_HEIGHT - 1) = Bishop(BOARD_WIDTH - 3, BOARD_HEIGHT - 1, WHITE)
        chessPieces(2)(0) = Bishop(2, 0, BLACK)
        chessPieces(BOARD_WIDTH - 3)(0) = Bishop(BOARD_WIDTH - 3, 0, BLACK)

        // kings and queens
        chessPieces(4)(BOARD_HEIGHT - 1) = King(4, BOARD_HEIGHT - 1, WHITE)
        chessPieces(3)(BOARD_HEIGHT - 1) = Queen(3, BOARD_HEIGHT - 1, WHITE)
        chessPieces(4)(0) = King(4, 0, BLACK)
        chessPieces(3)(0) = Queen(3, 0, BLACK)
        chessPieces

    def apply(): Board = {
        new standardBoard(standardBoard, false, false)
    }
}
