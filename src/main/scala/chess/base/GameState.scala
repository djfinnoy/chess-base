package chess.base


case class GameState(board: Chessboard, attr: GameAttributes) {

  def turnPieces(board: Chessboard, turn: Color): List[Xy] =
    board.filter(x => x._2.isDefined && x._2.get.color == turn).keys.toList

  def moveDestinations(board: Chessboard, turn: Color): List[List[Xy]] = {
    turnPieces(board, turn).map(x => board(x).get.moveSet(x, board))
  }

  def standardMoves: List[Move] = {
    def pieceMoves(origin: Xy, destinations: List[Xy]): List[Move] =
      destinations.map(x => Move(origin, x))
    def construct(orig: List[Xy], dest: List[List[Xy]], acc: List[Move]): List[Move] = {
      if (dest == Nil) acc else dest.head match {
        case Nil => construct(orig.tail, dest.tail, acc)
        case _ => construct(orig.tail, dest.tail, acc :::
          pieceMoves(orig.head, dest.head))
      }
    }
    construct(turnPieces(board, attr.turn), moveDestinations(board, attr.turn), Nil)
  }

  def enemyMoves(board: Chessboard, enemyColor: Color): List[Xy] =
    moveDestinations(board, enemyColor).flatten

  def enPassantMoves: List[Move] = if (attr.enPassant == None) Nil else {
    val x: Int = if (attr.turn == w) -1 else 1
    val origins: List[Xy] =
      Xy((attr.enPassant.get.col + x, attr.enPassant.get.row - 1)) ::
      Xy((attr.enPassant.get.col + x, attr.enPassant.get.row + 1)) :: Nil
    val left: List[Move] = if (board(origins(0)) == Some(Pawn(attr.turn)))
      List(Move(origins(0), attr.enPassant.get)) else Nil
    val right: List[Move] = if (board(origins(1)) == Some(Pawn(attr.turn)))
      List(Move(origins(1), attr.enPassant.get)) else Nil
    left ::: right
  }

  def castlingMoves: List[Move] = {
    lazy val row: Int = if (attr.turn == w) 1 else 8
    lazy val leftCastling: List[Move] = {
      val destination: Xy = Xy(3, row)
      if (unobstructedPath(destination) && safePath((destination))) {
        List(Move(Xy((5, row)), destination))
      } else Nil
    }
    lazy val rightCastling: List[Move] = {
      val destination: Xy = Xy(7, row)
      if (unobstructedPath(destination) && safePath((destination))) {
        List(Move(Xy((5, row)), destination))
      } else Nil
    }
    def unobstructedPath(destination: Xy): Boolean = {
      val col: Int = if (destination.col < 5) 4 else 6
      board(destination) == None && board(Xy(col, destination.row)) == None
    }
    def safePath(destination: Xy): Boolean = {
      val col: Int = if (destination.col < 5) 4 else 6
      kingSafe(board, attr.turn) &&
      List(destination, Xy((col, destination.row)))
        .intersect(enemyMoves(board, if (attr.turn == w) b else w)) == Nil
    }
    attr.noCastling(attr.turn) match {
      case (true, true) => Nil
      case (false, true) => leftCastling
      case (true, false) => rightCastling
      case (false, false) => leftCastling ::: rightCastling
    }
  }

  def kingSafe(board: Chessboard, color: Color): Boolean = {
    val kingPos: Xy = board.filter(x => x._2.getOrElse(None) == King(color)).keys.head
    !enemyMoves(board, if (color == w) b else w).contains(kingPos)
  }

  val validMoves: List[Move] = {
    val moves: List[Move] = (standardMoves ::: enPassantMoves ::: castlingMoves)
      .filter(x => kingSafe(x.newBoard, attr.turn))
    val promotionMoves: List[Move] = moves.filter(x => x.promotion)
    def modify(arg: List[Move]): List[Move] = {
      def recurse(x: List[Move], acc: List[Move]): List[Move] = x match {
        case Nil => acc
        case _ => recurse(x.tail, acc ::: List(
          x.head.copy(promotionPiece = Some(Queen(attr.turn))),
          x.head.copy(promotionPiece = Some(Rook(attr.turn))),
          x.head.copy(promotionPiece = Some(Bishop(attr.turn))),
          x.head.copy(promotionPiece = Some(Knight(attr.turn)))))
      }
      recurse(arg, Nil)
    }
    if (attr.fiftyRule) Nil else if (promotionMoves != Nil) {
      (moves diff promotionMoves) ::: modify(promotionMoves)
    } else moves
  }

  override def toString(): String = {
    def fetchUnicode(arg: Option[Piece]): String =
      if (arg == None) " " else arg.get.asUnicode
    "="*19 + "\n" +
    s"${if (attr.turn == w) "White" else "Black"} to move\n" +
    s"Valid moves: ${validMoves.length}\n" + "-"*19 +
    board.toList.sortBy(x => (-x._1.row, x._1.col)).foldLeft("\n8 ") { (agg, x) =>
      val next: String = x._1.col match {
        case 8 => s".${fetchUnicode(x._2)}.\n${x._1.row - 1} "
        case _ => s".${fetchUnicode(x._2)}"
      }
      agg + next
    }.dropRight(2) +
    "   a b c d e f g h\n" + "-"*19 + "\n" + attr + "="*19 + "\n"
  }

  case class Move(origin: Xy, destination: Xy, promotionPiece: Option[Piece] = None) {

    def newBoard: Chessboard = {
      def executeMove: Chessboard =
        board + (destination -> board(origin)) + (origin -> None)
      if (castling) {
        val rookDestination: Xy =
          if (destination.col == 7) Xy((6, origin.row)) else Xy((4, origin.row))
        val rookOrigin: Xy =
          if (destination.col == 7) Xy((8, origin.row)) else Xy((1, origin.row))
        executeMove + (rookDestination -> board(rookOrigin)) + (rookOrigin -> None)
      } else if (promotion) {
        board + (destination -> promotionPiece) + (origin -> None)
      } else if (enPassant) {
        executeMove + (Xy((destination.col, destination.row - 1)) -> None)
      } else {
        executeMove
      }
    }

    def newAttr: GameAttributes = GameAttributes(
      attr.moveCount + 1, check, noCastling, enPassantPos, lastPawnOrCapture)

    def implement: GameState = GameState(newBoard, newAttr)

    def castling: Boolean =
      board(origin).get == King(attr.turn) && Math.abs(origin.col - destination.col) > 1

    def promotion: Boolean =
      board(origin).get == Pawn(attr.turn) && List(1, 8).contains(destination.row)

    def check: Boolean = !kingSafe(newBoard, if (attr.turn == w) b else w)

    def capture: Boolean = board(destination).isDefined

    def enPassant: Boolean =
      origin.col != destination.col &&
      board(origin) == Some(Pawn(attr.turn)) && board(destination) == None

    def noCastling: Map[Color, (Boolean, Boolean)] = {
      if ((attr.noCastling(attr.turn)._1 && attr.noCastling(attr.turn)._2) ||
        !List(King(attr.turn), Rook(attr.turn)).contains(board(origin).get)) {
        attr.noCastling
      } else {
        origin.col match {
          case 1 => attr.noCastling + (attr.turn -> (true, attr.noCastling(attr.turn)._2))
          case 5 => attr.noCastling + (attr.turn -> (true, true))
          case 8 => attr.noCastling + (attr.turn -> (attr.noCastling(attr.turn)._1, true))
          case _ => throw new Exception
        }
      }
    }

    def enPassantPos: Option[Xy] = if (board(origin) == Some(Pawn(attr.turn)) &&
      Math.abs(origin.row - destination.row) == 2) {
        val diff: Int = if (attr.turn == w) -1 else 1
        Option(Xy(origin.col, destination.row + diff))
      } else None

    def lastPawnOrCapture: Int = if (board(origin).get == Pawn(attr.turn) ||
      capture) attr.moveCount + 1 else attr.lastPawnOrCapture
  }

  object Move {
    def apply(arg: String): GameState = {
      val promotionPiece: Option[Piece] = arg.last match {
        case 'q' => Some(Queen(attr.turn)); case 'n' => Some(Knight(attr.turn))
        case 'b' => Some(Bishop(attr.turn)); case 'r' => Some(Rook(attr.turn))
        case _ => None
      }
      val desiredMove: Move = Move(Xy(arg.take(2)), Xy(arg.slice(2,4)), promotionPiece)
      if (validMoves.contains(desiredMove)) desiredMove.implement else
        throw new InvalidMoveException
    }
  }
}

case class GameAttributes(
  moveCount: Int,
  check: Boolean,
  noCastling: Map[Color, (Boolean, Boolean)],
  enPassant: Option[Xy],
  lastPawnOrCapture: Int) {
  def turn: Color = if (this.moveCount % 2 == 0) w else b
  def fiftyCount: Int = moveCount - lastPawnOrCapture
  def fiftyRule: Boolean = fiftyCount >= 30
  override def toString(): String = {
    s"Move #: ${moveCount + 1}\nCheck: ${check}\nNo castling: ${noCastling}" +
    s"\nEnpassant: ${enPassant}\n50-rule counter: ${fiftyCount}\n"
  }
}

object newGame {
  def apply() = GameState(startPosition, startAttributes)
  def startAttributes =
    GameAttributes(0, false, Map(w -> (false, false), b -> (false, false)), None, 0)
  def startPosition: Chessboard = {
    def place(arg: List[(Xy, Option[Piece])], board: Chessboard): Chessboard = {
      arg match {
        case Nil => board
        case _ => place(arg.tail, board + (arg.head._1 -> arg.head._2))
      }
    }
    val pieces: List[(Xy, Option[Piece])] = for {
      c <- List("a","b","c","d","e","f","g","h")
      r <- List(1,2,7,8)
    } yield (r, c) match {
      case (1, "a"|"h") => (Xy(c+r), Some(Rook(w)))
      case (8, "a"|"h") => (Xy(c+r), Some(Rook(b)))
      case (1, "b"|"g") => (Xy(c+r), Some(Knight(w)))
      case (8, "b"|"g") => (Xy(c+r), Some(Knight(b)))
      case (1, "c"|"f") => (Xy(c+r), Some(Bishop(w)))
      case (8, "c"|"f") => (Xy(c+r), Some(Bishop(b)))
      case (1, "d") => (Xy(c+r), Some(Queen(w)))
      case (8, "d") => (Xy(c+r), Some(Queen(b)))
      case (1, "e") => (Xy(c+r), Some(King(w)))
      case (8, "e") => (Xy(c+r), Some(King(b)))
      case (2, _) => (Xy(c+r), Some(Pawn(w)))
      case (7, _) => (Xy(c+r), Some(Pawn(b)))
      case _ => throw new Exception
    }
    place(pieces, squares)
  }
}
