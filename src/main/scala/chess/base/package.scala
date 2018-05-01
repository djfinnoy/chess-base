package chess


package object base {

  sealed trait Color
  case object w extends Color
  case object b extends Color

  type Chessboard = Map[Xy, Option[Piece]]

  val squares: Chessboard = (for {
    c <- List("a","b","c","d","e","f","g","h")
    r <- 1 to 8
  } yield (Xy(c + r), None)).toMap

  case class Xy(arg: String) {
    val cols = List('a','b','c','d','e','f','g','h')
    def col: Int = cols.indexOf(arg.head) + 1
    def row: Int = arg.last.toInt - 48
    def notation: String = cols(col - 1).toString + row
  }

  object Xy {
    def apply(arg: (Int, Int)): Xy = {
      val cols = List('a','b','c','d','e','f','g','h')
      if (! (1 to 8).exists(_ == arg._1)) {
        new Xy("a0")
      } else new Xy(cols(arg._1 - 1).toString + arg._2)
    }
  }

  class InvalidMoveException extends Exception("Invalid move")
}



