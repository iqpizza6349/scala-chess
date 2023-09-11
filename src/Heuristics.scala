package org.iqpizza

import Type.*

import PieceColor.WHITE

object Heuristics {
    private val PAWN_TABLE = Array(
        Array(0,   0,   0,   0,   0,   0,   0,   0),
        Array(5,  10,  10, -20, -20,  10,  10,   5),
        Array(5,  -5, -10,   0,   0, -10,  -5,   5),
        Array(0,   0,   0,  20,  20,   0,   0,   0),
        Array(5,   5,   10, 25,  25,   10,  5,   5),
        Array(10,  10,  20, 30,  30,   20,  10,  10),
        Array(50,  50,  50, 50,  50,   50,  50,  50),
        Array(0,   0,   0,   0,   0,   0,   0,   0)
    )

    private val KNIGHT_TABLE = Array(
        Array(-50, -40, -30, -30, -30, -30, -40, -50),
        Array(-40, -20,   0,   5,   5,   0, -20, -40),
        Array(-30,   5,  10,  15,  15,  10,   5, -30),
        Array(-30,   0,  15,  20,  20,  15,   0, -30),
        Array(-30,   5,  15,  20,  20,  15,   0, -30),
        Array(-30,   0,  10,  15,  15,  10,   0, -30),
        Array(-40, -20,   0,   0,   0,   0, -20, -40),
        Array(-50, -40, -30, -30, -30, -30, -40, -50)
    )

    private val BISHOP_TABLE = Array(
        Array(-20, -10, -10, -10, -10, -10, -10, -20),
        Array(-10,   5,   0,   0,   0,   0,   5, -10),
        Array(-10,  10,  10,  10,  10,  10,  10, -10),
        Array(-10,   0,  10,  10,  10,  10,   0, -10),
        Array(-10,   5,   5,  10,  10,   5,   5, -10),
        Array(-10,   0,   5,  10,  10,   5,   0, -10),
        Array(-10,   0,   0,   0,   0,   0,   0, -10),
        Array(-20, -10, -10, -10, -10, -10, -10, -20)
    )

    private val ROOK_TABLE  = Array(
        Array(0,  0,  0,  5,  5,  0,  0,  0),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(5, 10, 10, 10, 10, 10, 10,  5),
        Array( 0,  0,  0,  0,  0,  0,  0,  0)
    )


    private val QUEEN_TABLE  = Array(
        Array(-20, -10, -10, -5, -5, -10, -10, -20),
        Array(-10,   0,   5,  0,  0,   0,   0, -10),
        Array(-10,   5,   5,  5,  5,   5,   0, -10),
        Array(0,   0,   5,  5,  5,   5,   0,  -5),
        Array(-5,   0,   5,  5,  5,   5,   0,  -5),
        Array(-10,   0,   5,  5,  5,   5,   0, -10),
        Array(-10,   0,   0,  0,  0,   0,   0, -10),
        Array(-20, -10, -10, -5, -5, -10, -10, -20)
    )

    def evaluate(board: Board): Int =
        val material = getMaterialScore(board)

        val pawns = getPiecePositionScore(board, PAWN, PAWN_TABLE)
        val knights = getPiecePositionScore(board, KNIGHT, KNIGHT_TABLE)
        val bishops = getPiecePositionScore(board, BISHOP, BISHOP_TABLE)
        val rooks = getPiecePositionScore(board, ROOK, ROOK_TABLE)
        val queens = getPiecePositionScore(board, QUEEN, QUEEN_TABLE)
        material + pawns + bishops + rooks + queens

    private def getPiecePositionScore(board: Board, pieceType: Type, table: Array[Array[Int]]): Int =
        var white, black = 0
        for (x <- 0 until 8)
            for (y <- 0 until 8)
                val piece = board.pieces(x)(y)
                if piece != null then
                    if piece.pieceType == pieceType then
                        if  piece.color == WHITE then white += table(x)(y)
                        else black += table(x)(y)
        white + black

    private def getMaterialScore(board: Board): Int =
        var white, black = 0
        for (x <- 0 until 8)
            for (y <- 0 until 8)
                val piece = board.pieces(x)(y)
                if piece != null then
                    if piece.color == WHITE then white += piece.worth
                    else black += piece.worth
        white - black
}