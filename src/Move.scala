package org.iqpizza

import java.util.Objects

class Move(val xFrom: Int, val yFrom: Int, val xTo: Int, val yTo: Int) {

    override def equals(obj: Any): Boolean = obj match {
        case that: Move => (this.xFrom == that.xFrom)
            && (this.yFrom == that.yFrom)
            && (this.xTo == that.xTo)
            && (this.yTo == that.yTo)
        case _ => false
    }

    override def hashCode(): Int = Objects.hash(xFrom, yFrom, xTo, yTo)

    override def toString: String =
        "(" + this.xFrom + ", " + this.yFrom + ") -> "
            + "(" + this.xTo + ", " + this.yTo + ")"
}
