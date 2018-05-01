package chess.base


trait Piece {
  val color: Color
  def asUnicode: String
  def moveSet(origin: Xy, board: Chessboard): List[Xy]
}

case class Rook(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2656"; case `b` => "\u265C"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = { recDirCheck(origin, left, color, board) :::
    recDirCheck(origin, right, color, board) :::
    recDirCheck(origin, up, color, board) :::
    recDirCheck(origin, down, color, board)
  }
}

case class Knight(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2658"; case `b` => "\u265E"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = {
    List(
      up(up(left(origin))), up(up(right(origin))),
      right(right(up(origin))), right(right(down(origin))),
      down(down(right(origin))), down(down(left(origin))),
      left(left(down(origin))), left(left(up(origin))))
        .filter(x => emptyPos(x, board) || enemyPos(x, color, board))
  }
}

case class Bishop(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2657"; case `b` => "\u265D"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = {
    recDirCheck(origin, upLeft, color, board) :::
    recDirCheck(origin, upRight, color, board) :::
    recDirCheck(origin, downLeft, color, board) :::
    recDirCheck(origin, downRight, color, board)
  }
}

case class Queen(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2655"; case `b` => "\u265B"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = {
    recDirCheck(origin, up, color, board) :::
    recDirCheck(origin, down, color, board) :::
    recDirCheck(origin, left, color, board) :::
    recDirCheck(origin, right, color, board) :::
    recDirCheck(origin, upLeft, color, board) :::
    recDirCheck(origin, upRight, color, board) :::
    recDirCheck(origin, downLeft, color, board) :::
    recDirCheck(origin, downRight, color, board)
  }
}

case class King(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2654"; case `b` => "\u265A"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = {
    List(
      upLeft(origin), up(origin), upRight(origin), right(origin),
      downRight(origin), down(origin), downLeft(origin), left(origin))
        .filter(x => emptyPos(x, board) || enemyPos(x, color, board))
  }
}

case class Pawn(color: Color) extends Piece with MoveFunc {
  def asUnicode: String = color match {case `w` => "\u2659"; case `b` => "\u265F"}
  def moveSet(origin: Xy, board: Chessboard): List[Xy] = {
    def forward(origin: Xy): Xy = if (color == w) up(origin) else down(origin)
    val fwdOne: (Xy => Xy) = (x: Xy) => (forward(x))
    val fwdTwo: (Xy => Xy) = (x: Xy) => (forward(forward(x)))
    val diagL:  (Xy => Xy) = (x: Xy) => left(forward(x))
    val diagR:  (Xy => Xy) = (x: Xy) => right(forward(x))
    def isValid(arg: (Xy => Xy)): Boolean = arg match {
      case x if x == fwdOne => emptyPos(fwdOne(origin), board)
      case x if x == fwdTwo =>
        emptyPos(fwdOne(origin), board) && emptyPos(fwdTwo(origin), board) &&
        (origin.row == 2 && color == w || origin.row == 7 && color == b)
      case x if x == diagL && origin.col != 1 => enemyPos(diagL(origin), color, board)
      case x if x == diagR && origin.col != 8 => enemyPos(diagR(origin), color, board)
      case _ => false
    }
    List(fwdOne, fwdTwo, diagL, diagR).filter(isValid).map(x => x(origin))
  }
}

trait MoveFunc {
  def up(origin: Xy): Xy = Xy((origin.col, origin.row + 1))
  def down(origin: Xy): Xy = Xy((origin.col, origin.row - 1))
  def left(origin: Xy): Xy = Xy((origin.col - 1, origin.row))
  def right(origin: Xy): Xy = Xy((origin.col + 1, origin.row))
  def upLeft(origin: Xy): Xy = up(left(origin))
  def upRight(origin: Xy): Xy = up(right(origin))
  def downLeft(origin: Xy): Xy = down(left(origin))
  def downRight(origin: Xy): Xy = down(right(origin))
  def validPos(pos: Xy): Boolean = squares.contains(pos)
  def emptyPos(pos: Xy, board: Chessboard): Boolean =
    validPos(pos) && board(pos) == None
  def enemyPos(pos: Xy, friendlyColor: Color, board: Chessboard): Boolean = {
    if (!validPos(pos)) false else board(pos) match {
      case Some(x) if x.color != friendlyColor => true
      case _ => false
    }
  }
  def recDirCheck(origin: Xy, dir: (Xy => Xy),
    friendlyColor: Color, board: Chessboard): List[Xy] = {

    def recCheck(next: Xy, acc: List[Xy]): List[Xy] = next match {
      case x if enemyPos(x, friendlyColor, board) => acc ::: x :: Nil
      case x if emptyPos(x, board) => recCheck(dir(x), acc ::: x :: Nil)
      case _ => acc
    }
    recCheck(dir(origin), Nil)
  }
}

