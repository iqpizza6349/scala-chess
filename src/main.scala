package org.iqpizza

@main
def main(): Unit = {
  val moveA: Move = Move(0, 0, 1, 0)
  println(moveA)
  val moveB: Move = Move(0, 0, 1, 0)
  println(moveB)
  println(moveA.equals(moveB))
  println(moveA.eq(moveB))
  println(moveA == moveB)
}
