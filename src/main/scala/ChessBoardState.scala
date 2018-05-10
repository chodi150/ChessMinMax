
class ChessBoardState(val playerOnePositions : Set[Position],
                      val playerTwoPositions : Set[Position],
                      val availablePositions : Set[Position]) {
  final val DIMENSION = 8

  lazy val isGameOver: Boolean = playerOneLose || playerTwoLose
  lazy val playerOneLose: Boolean = checkLose(playerOnePositions)
  lazy val playerTwoLose: Boolean = checkLose(playerTwoPositions)

  def checkLose(positions: Set[Position]): Boolean = !positions.toStream.exists(p => p.figure == 6)

  def makeMove(p1: Position, p2: Position): ChessBoardState = {
      val p = playerTwoPositions.toStream.find(p => p.equalCoords(p2)).orNull
      if (p != null) {
        new ChessBoardState(reverseChessBoard(playerTwoPositions - p),reverseChessBoard(playerOnePositions - p1 + p2), reverseChessBoard(availablePositions + Position(p1.row, p1.col, 0)))
      }
      else {
        new ChessBoardState(reverseChessBoard(playerTwoPositions),reverseChessBoard(playerOnePositions - p1 + p2), reverseChessBoard(availablePositions + Position(p1.row, p1.col, 0) - p2))
      }
  }

    def display(): Unit = {
      val chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, -p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,p.figure*10))
      val chessBoardSort = collection.immutable.SortedSet[Position]() ++ chessBoard
      chessBoardSort.grouped(8).foreach(x => {x.foreach(y=>print(y.figure+" ")); println("")})
    }

  def generateStatesForKing(position: Position) : Set[ChessBoardState] = {
    val possibleMoves: Set[Position] = generatePositionsForKing(position)
      possibleMoves.map(x => makeMove(position, x))

  }

  def generatePositionsForKing(position: Position): Set[Position] = {
    val pseudoPossibleMoves = Range(position.row - 1, position.row + 2)
      .flatMap(x => Range(position.col - 1, position.col + 2)
        .map(y => Position(x, y, 6))).filter(x => x != position && x.positionOnBoard()).toSet
    val possibleMoves = pseudoPossibleMoves -- playerOnePositions
    possibleMoves
  }

  def generateStatesForBishop(position: Position) : Set[ChessBoardState] = {
    val bishopMoves = generatePositionsForBishop(position)
    bishopMoves.map(x=>makeMove(position,x))
  }

  def generatePositionsForBishop(position: Position): Set[Position] = {
    tryToMoveInDirection(position, 1, 1) ++ tryToMoveInDirection(position, 1, -1) ++ tryToMoveInDirection(position, -1, 1) ++ tryToMoveInDirection(position, -1, -1)
  }

  def generateStatesForRook(position: Position) : Set[ChessBoardState] = {
    val rookMoves = generatePositionsForRook(position)
    rookMoves.map(x=>makeMove(position,x))
  }

  def generatePositionsForRook(position: Position): Set[Position] = {
    tryToMoveInDirection(position, 0, 1) ++ tryToMoveInDirection(position, 1, 0) ++ tryToMoveInDirection(position, 0, -1) ++ tryToMoveInDirection(position, -1, 0)
  }

  def generateStatesForQueen(position: Position) : Set[ChessBoardState] = {
    generateStatesForBishop(position) ++ generateStatesForRook(position)
  }
  def tryToMoveInDirection(position: Position, colMove : Int, rowMove : Int) : Set[Position] = {
    val newPosition = Position(position.row + colMove, position.col + rowMove, position.figure)
    if (!newPosition.positionOnBoard())
      return Set()

    if (playerOnePositions contains newPosition)
      Set()
    else if (playerTwoPositions contains newPosition)
      Set(newPosition)
    else
      Set(newPosition) ++ tryToMoveInDirection(newPosition, colMove, rowMove)

  }


    def generateStatesForPawn(position: Position) : Set[ChessBoardState] = {
        val possiblePositions = Set.empty ++ availablePositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col, position.figure))).toSet ++ playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col+1, position.figure))).toSet ++ playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col-1, position.figure))).toSet
        possiblePositions.map(p => makeMove(position, Position(p.row, p.col, position.figure)))
    }

    def reverseChessBoard(positions: Set[Position]) : Set[Position] = {
      positions.map(p => Position(7-p.col, 7-p.row, p.figure))
    }

    def generateStatesForHorse(position: Position) : Set[ChessBoardState] = {
      val possibleMoves: Set[Position] = generatePositionsForHorse(position)
      possibleMoves.map(p => makeMove(position,p))
    }

  def generatePositionsForHorse(position: Position): Set[Position] = {
    val horseMove1 = Position(position.row + 2, position.col + 1, position.figure)
    val horseMove2 = Position(position.row + 2, position.col - 1, position.figure)
    val horseMove3 = Position(position.row - 2, position.col + 1, position.figure)
    val horseMove4 = Position(position.row - 2, position.col - 1, position.figure)
    val horseMove5 = Position(position.row + 1, position.col + 2, position.figure)
    val horseMove6 = Position(position.row + 1, position.col - 2, position.figure)
    val horseMove7 = Position(position.row - 1, position.col + 2, position.figure)
    val horseMove8 = Position(position.row - 1, position.col - 2, position.figure)
    val pseudoPossibleMoves = Set(horseMove1, horseMove2, horseMove3, horseMove4, horseMove5, horseMove6, horseMove6, horseMove7, horseMove8)
    val possibleMoves = pseudoPossibleMoves.toStream.filter(p => availablePositions(p) || playerTwoPositions(p)).toSet
    possibleMoves
  }

  def generateStatesForPosition(position: Position) : Set[ChessBoardState] =  position.figure match {
      case 1 => generateStatesForPawn(position)
      case 2 => generateStatesForHorse(position)
      case 3 => generateStatesForBishop(position)
      case 4 => generateStatesForRook(position)
      case 5 => generateStatesForQueen(position)
      case 6 => generateStatesForKing(position)
    }

  def countScoreForFigure(figure : Int): Int = figure match {
    case 1 => 1
    case 2 => 3
    case 3 => 3
    case 4 => 5
    case 5 => 9
    case 6 => 100
  }

  def countScoreForChessState(isMaximizingPlayer: Boolean) : Int = {
    if(isMaximizingPlayer){
      playerTwoPositions.map(p=>countScoreForFigure(p.figure)).sum - playerOnePositions.map(p=>countScoreForFigure(p.figure)).sum
    }
    else{
      playerOnePositions.map(p=>countScoreForFigure(p.figure)).sum - playerTwoPositions.map(p=>countScoreForFigure(p.figure)).sum
    }
  }

  def minimax(isMaximizingPlayer : Boolean, depth:Int) : ChessBoardState = {
    if (depth==0 || isGameOver) {
      if (isMaximizingPlayer) {
        playerOnePositions.flatMap(p => generateStatesForPosition(p)).reduceLeft((x, y) => if (x.countScoreForChessState(true) > y.countScoreForChessState(true)) x else y)
      }
      else {
        playerOnePositions.flatMap(p => generateStatesForPosition(p)).reduceLeft((x, y) => if (x.countScoreForChessState(false) < y.countScoreForChessState(false)) x else y)
      }
    }
    else{
      if (isMaximizingPlayer) {
        val possibleStates = playerOnePositions.flatMap(p => generateStatesForPosition(p)).map(x => x.minimax(false,depth-1))
        possibleStates.reduceLeft((x, y) => if (x.countScoreForChessState(true) > y.countScoreForChessState(true)) x else y)
      }
      else {
        val possibleStates = playerOnePositions.flatMap(p => generateStatesForPosition(p)).map(x => x.minimax(true,depth-1))
        possibleStates.reduceLeft((x, y) => if (x.countScoreForChessState(false) < y.countScoreForChessState(false)) x else y)

      }
    }
  }

}

