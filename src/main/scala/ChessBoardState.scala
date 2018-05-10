
class ChessBoardState(val playerOnePositions : Set[Position],
                      val playerTwoPositions : Set[Position],
                      val availablePositions : Set[Position],
                      val isPlayerOneMove : Boolean) {
  final val DIMENSION = 8

  lazy val isGameOver: Boolean = playerOneLose || playerTwoLose
  lazy val playerOneLose: Boolean = checkLose(playerOnePositions)
  lazy val playerTwoLose: Boolean = checkLose(playerTwoPositions)

  def checkLose(positions: Set[Position]): Boolean = !positions.toStream.exists(p => p.figure == 6)

  def makeMove(p1: Position, p2: Position): ChessBoardState = {
    if (isPlayerOneMove) {
      val p = playerTwoPositions.toStream.find(p => p.equalCoords(p2)).orNull
      if (p != null) {
        new ChessBoardState(playerOnePositions - p1 + p2, playerTwoPositions - p, availablePositions + Position(p1.row, p1.col, 0), false)
      }
      else {
        new ChessBoardState(playerOnePositions - p1 + p2, playerTwoPositions, availablePositions + Position(p1.row, p1.col, 0) - p2, false)
      }
    }
    else {
      val p = playerOnePositions.toStream.find(p => p.equalCoords(p2)).orNull
      if (p != null) {
        new ChessBoardState(playerOnePositions - p, playerTwoPositions - p1 + p2, availablePositions + Position(p1.row, p1.col, 0), true)
      }
      else {
        new ChessBoardState(playerOnePositions, playerTwoPositions - p1 + p2, availablePositions + Position(p1.row, p1.col, 0) - p2, true)
      }
    }
  }

    def display(): Unit = {
      val chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, -p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,p.figure*10))
      val chessBoardSort = collection.immutable.SortedSet[Position]() ++ chessBoard
      chessBoardSort.grouped(8).foreach(x => {x.foreach(y=>print(y.figure+" ")); println("")})
    }

  def generateStatesForKing(position: Position) : Set[ChessBoardState] = {
    val pseudoPossibleMoves = Range(position.row - 1, position.row + 2)
      .flatMap(x => Range(position.col - 1, position.col + 2)
        .map(y => Position(x, y, 6))).filter(x => x != position && x.positionOnBoard()).toSet
    if(isPlayerOneMove){
      val possibleMoves = pseudoPossibleMoves -- playerOnePositions
      possibleMoves.map(x => makeMove(position, x))
    }
    else{
      val possibleMoves = pseudoPossibleMoves -- playerTwoPositions
      possibleMoves.map(x => makeMove(position, x))
    }
  }

//  def generateStatesForBishop(position:Position) : Set[ChessBoardState] = {
//    val pseudoPossibleMovesRD = Range(1,8).map(x => Position(position.row + x, position.col+x, 3)).takeWhile(x => !playerOnePositions(x)).filter(x => x.positionOnBoard()).toSet
//    val movesBehindFirstEnemyRD = Range(1,8).map(x => Position(position.row + x, position.col+x, 3)).dropWhile(x => !playerTwoPositions(x)).drop(1).filter(x => x.positionOnBoard()).toSet
//    val possibleMovesRD = pseudoPossibleMovesRD -- movesBehindFirstEnemyRD
//
//    val pseudoPossibleMovesRU = Range(1,8).map(x => Position(position.row - x, position.col+x, 3)).takeWhile(x => !playerOnePositions(x)).filter(x => x.positionOnBoard()).toSet
//    val movesBehindFirstEnemyRU = Range(1,8).map(x => Position(position.row - x, position.col+x, 3)).dropWhile(x => !playerTwoPositions(x)).drop(1).filter(x => x.positionOnBoard()).toSet
//    val possibleMovesRU = pseudoPossibleMovesRU -- movesBehindFirstEnemyRU
//
//    val pseudoPossibleMovesLD = Range(1,8).map(x => Position(position.row + x, position.col-x, 3)).takeWhile(x => !playerOnePositions(x)).filter(x => x.positionOnBoard()).toSet
//    val movesBehindFirstEnemyLD = Range(1,8).map(x => Position(position.row + x, position.col-x, 3)).dropWhile(x => !playerTwoPositions(x)).drop(1).filter(x => x.positionOnBoard()).toSet
//    val possibleMovesLD = pseudoPossibleMovesRU -- movesBehindFirstEnemyRU
//
//    val pseudoPossibleMovesLU = Range(1,8).map(x => Position(position.row - x, position.col-x, 3)).takeWhile(x => !playerOnePositions(x)).filter(x => x.positionOnBoard()).toSet
//    val movesBehindFirstEnemyLU = Range(1,8).map(x => Position(position.row - x, position.col-x, 3)).dropWhile(x => !playerTwoPositions(x)).drop(1).filter(x => x.positionOnBoard()).toSet
//    val possibleMovesLU = pseudoPossibleMovesRU -- movesBehindFirstEnemyRU
//
//    (possibleMovesLD ++ possibleMovesLU ++ possibleMovesRD ++ possibleMovesRU).map( x => makeMove(position, x))
//
//  }

  def generateStatesForBishop(position: Position) : Set[ChessBoardState] = {
    val bishopMoves = tryToMoveInDirection(position,1,1) ++ tryToMoveInDirection(position,1,-1) ++ tryToMoveInDirection(position,-1,1) ++ tryToMoveInDirection(position,-1,-1)
    bishopMoves.map(x=>makeMove(position,x))
  }
  def generateStatesForRook(position: Position) : Set[ChessBoardState] = {
    val rookMoves = tryToMoveInDirection(position,0,1) ++ tryToMoveInDirection(position,1,0) ++ tryToMoveInDirection(position,0,-1) ++ tryToMoveInDirection(position,-1,0)
    rookMoves.map(x=>makeMove(position,x))
  }
  def generateStatesForQueen(position: Position) : Set[ChessBoardState] = {
    generateStatesForBishop(position) ++ generateStatesForRook(position)
  }
  def tryToMoveInDirection(position: Position, colMove : Int, rowMove : Int) : Set[Position] = {
    val newPosition=Position(position.row+1, position.col+1, position.figure)
    if(!newPosition.positionOnBoard())
      return Set()
    val playerTwoExists = playerTwoPositions.toStream.exists(p => p.equalCoords(newPosition))
    val playerOneExists = playerOnePositions.toStream.exists(p => p.equalCoords(newPosition))
    if(isPlayerOneMove){
      if(playerOneExists)
        Set()
      else if(playerTwoExists)
        Set(newPosition)
      else
        Set(newPosition)++tryToMoveInDirection(newPosition, colMove, rowMove)
    }
    else{
      if(playerTwoExists)
        Set()
      else if(playerOneExists)
        Set(newPosition)
      else
        Set(newPosition)++tryToMoveInDirection(newPosition, colMove, rowMove)
    }

  }

}
