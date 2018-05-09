
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

//  def generateStatesForPawn(position: Position, chessBoardState: ChessBoardState, isPlayerOneMove: Boolean) : Set[ChessBoardState] = {
//    if(isPlayerOneMove){
//
//    }
//    else{
//
//    }
//  }

}
