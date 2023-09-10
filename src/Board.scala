package org.iqpizza

import PieceColor.{BLACK, WHITE}
import Type.{KING, PAWN}

class Board(var chessPieces: Array[Array[Piece]], var whiteKingMoved: Boolean,
            var blackKingMoved: Boolean) {

    def cloneAsOther(other: Board): Board =
        chessPieces = Array.fill(game.BOARD_WIDTH, game.BOARD_HEIGHT) { null }
        for (x <- 0 to game.BOARD_WIDTH)
            for (y <- 0 to game.BOARD_HEIGHT)
                val piece = other.chessPieces(x)(y)
                if piece != null then
                    chessPieces(x)(y) = piece.clone().asInstanceOf[Piece]

        new Board(chessPieces, this.whiteKingMoved, this.blackKingMoved)


    def getPiece(x: Int, y: Int): Piece =
        if !inBounds(x, y) then
            null
        else
            chessPieces(x)(y)

    def inBounds(x: Int, y: Int): Boolean =
        (x >= 0 && y >= 0) && (x < game.BOARD_WIDTH && y < game.BOARD_HEIGHT)

    def createBoard(): Board =
        var chessPieces = Array.ofDim[Piece](game.BOARD_WIDTH, game.BOARD_HEIGHT)

        //TODO: piece children class

        new Board(chessPieces, false, false)

    def getPossibleMoves(color: PieceColor): List[Move] =
        val moves = List[Move]()
        for (x <- 0 to game.BOARD_WIDTH)
            for (y <- 0 to game.BOARD_HEIGHT)
                val piece = this.chessPieces(x)(y)
                if piece != null then
                    if piece.color == color then
                        moves.appendedAll(piece.getPossibleMoves(this))
        moves

    def performMove(move: Move): Unit = {
        val piece = chessPieces(move.xFrom)(move.yFrom)
        movePiece(piece, move.xTo, move.yTo)

        if piece.pieceType == PAWN then
            if piece.y == 0 || piece.y == game.BOARD_HEIGHT - 1 then
                throw UnsupportedOperationException("Not Implemented that pawn is change to queen")
//                chessPieces(piece.x)(piece.y) =
        if piece.pieceType == KING then
            // mark the king as having moved.
            if piece.color == WHITE then
                whiteKingMoved = true
            else
                blackKingMoved = true

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

        for (mv <- getPossibleMoves(otherColor))
            val copy = cloneAsOther(this)
            copy.performMove(mv)

            var kingFound = false
            for (x <- 0 to game.BOARD_WIDTH)
                for (y <- 0 to game.BOARD_HEIGHT)
                    val piece = copy.chessPieces(x)(y)
                    if piece != null then
                        if piece.color == color && piece.pieceType == KING then
                            kingFound = true

            if !kingFound then
                return true
        false
}
