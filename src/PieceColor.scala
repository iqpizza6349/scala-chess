package org.iqpizza

enum PieceColor(val shortcut: String) extends Enum[PieceColor]{
    case WHITE extends PieceColor("W")
    case BLACK extends PieceColor("B")
}
