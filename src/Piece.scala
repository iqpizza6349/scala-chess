package org.iqpizza

enum Type {
    case PAWN, KING, QUEEN, BISHOP, KNIGHT, ROOK
}

abstract class Piece(var x: Int, var y: Int, val color: PieceColor,
                     val pieceType: Type, val worth: Int) extends Cloneable {



    def getPossibleDiagonalMoves(): List[Move] = null

    def getPossibleHorizontalMoves(): List[Move] = null

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
