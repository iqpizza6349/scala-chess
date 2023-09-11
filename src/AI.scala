package org.iqpizza

import Heuristics.evaluate
import PieceColor.{BLACK, WHITE}

import scala.util.boundary
import scala.util.control.Breaks

object AI {
    private val INFINITE = 10_000_000

    def getBestMove(chessboard: Board, invalidMoves: List[Move]): Move =
        var bestMove: Move = null
        var bestScore = INFINITE
        val loop = Breaks
        for (move <- chessboard.getPossibleMoves(BLACK))
            loop.breakable {
                if isInvalidMove(move, invalidMoves) then loop.break()

                val copy = chessboard.copy()

                copy.performMove(move)
                val score = alphaBeta(copy, 2, -INFINITE, INFINITE, true)
                if score < bestScore then
                    bestScore = score
                    bestMove = move
            }

        // checkmate.
        if bestMove == null then
            return null

        val copy = chessboard.copy()
        copy.performMove(bestMove)
        if copy.isCheck(BLACK) then
            getBestMove(chessboard, invalidMoves.appended(bestMove))
        bestMove


    private def isInvalidMove(move: Move, invalidMoves: List[Move]): Boolean =
        boundary:
            for (invalidMove <- invalidMoves)
                if invalidMove == move then boundary.break(true)
            false

//    def miniMax

    private def alphaBeta(chessboard: Board, depth: Int, a: Int, b: Int, maximizing: Boolean): Int =
        if depth == 0 then return evaluate(chessboard)

        var bestScore = INFINITE
        val loop = Breaks
        if maximizing then
            bestScore = -INFINITE
            loop.breakable {
                for (move <- chessboard.getPossibleMoves(WHITE))
                    val copy = chessboard.copy()
                    copy.performMove(move)

                    bestScore = Math.max(bestScore, alphaBeta(copy, depth - 1, a, b, false))
                    val newA = Math.max(a, bestScore)
                    if b <= newA then loop.break()
            }
        else
            loop.breakable {
                for (move <- chessboard.getPossibleMoves(BLACK))
                    val copy = chessboard.copy()
                    copy.performMove(move)

                    bestScore = Math.min(bestScore, alphaBeta(copy, depth - 1, a, b, true))
                    val newB = Math.min(b, bestScore)
                    if newB <= a then loop.break()
            }
        bestScore
}
