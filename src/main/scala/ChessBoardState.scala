
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

    def display(playedComputer:Boolean): Unit = {
      var chessBoard:Set[Position] = Set()
      if(playedComputer){
         chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, -p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,p.figure*10))
      }
      else{
         chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, 10*p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,-p.figure))
      }
     // val chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, -p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,p.figure*10))
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
      positions.map(p => Position(7-p.row, 7-p.col, p.figure))
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
    case 6 => 1000
  }

  def countScoreForChessState(isMaximizingPlayer: Boolean) : Int = {
    if(isMaximizingPlayer){
      playerTwoPositions.toStream.map(p=>countScoreForFigure(p.figure)).sum - playerOnePositions.toStream.map(p=>countScoreForFigure(p.figure)).sum
    }
    else{
      playerOnePositions.toStream.map(p=>countScoreForFigure(p.figure)).sum - playerTwoPositions.toStream.map(p=>countScoreForFigure(p.figure)).sum
    }
  }

 // def nextState(isMaximizingPlayer : Boolean, depth:Int):ChessBoardState = minimax(isMaximizingPlayer, depth)._1

//  def minimax(isMaximizingPlayer : Boolean, depth:Int) : (ChessBoardState, ChessBoardState)  = {
//    if (depth==0 || isGameOver) {
//      if (isMaximizingPlayer) {
//        val bestChild = playerOnePositions.flatMap(p => generateStatesForPosition(p)).reduceLeft((x, y) => if (x.countScoreForChessState(true) > y.countScoreForChessState(true)) x else y)
//        (this, bestChild)
//      }
//      else {
//        val bestChild = playerOnePositions.flatMap(p => generateStatesForPosition(p)).reduceLeft((x, y) => if (x.countScoreForChessState(false) < y.countScoreForChessState(false)) x else y)
//        (this,bestChild)
//      }
//    }
//    else{
//      if (isMaximizingPlayer) {
//       playerOnePositions.flatMap(p => generateStatesForPosition(p)).map(x => x.minimax(false,depth-1)).
//          reduceLeft((x,y)=> if(x._2.countScoreForChessState(true) > y._2.countScoreForChessState(true)) x else y)
//
//      }
//      else {
//        val possibleStates = playerOnePositions.flatMap(p => generateStatesForPosition(p)).map(x => x.minimax(true,depth-1))
//        possibleStates.reduceLeft((x, y) => if (x._2.countScoreForChessState(false) < y._2.countScoreForChessState(false)) x else y)
//
//      }
//    }
//  }

  def nextState(isMaximizingPlayer : Boolean, depth:Int):ChessBoardState = {
    val allChildren = allPossibleUpcomingStates
    val allChildrenScores = allChildren.map(x => x.minimax(isMaximizingPlayer, depth -1))
    if (isMaximizingPlayer) {
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 > y._2) x else y)
      bestStateTuple._1
    }else{
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 < y._2) x else y)
      bestStateTuple._1
    }
  }


  def allPossibleUpcomingStates: Set[ChessBoardState] = {
    playerOnePositions.flatMap(p => generateStatesForPosition(p))
  }

  def minimax(isMaximizingPlayer : Boolean, depth:Int) : (ChessBoardState, Int)  = {
    if (depth==0 || isGameOver) {
        (this, countScoreForChessState(isMaximizingPlayer))
    }
    else{
      if (isMaximizingPlayer) {
        val allChildren = playerOnePositions.flatMap(p => generateStatesForPosition(p))
        val allChildrenScores = allChildren.map(x => x.minimax(!isMaximizingPlayer, depth -1))
        allChildrenScores.reduceLeft((x,y) => if(x._2 < y._2) (this, x._2) else (this, y._2))
      }
      else {
        val allChildren = playerOnePositions.flatMap(p => generateStatesForPosition(p))
        val allChildrenScores = allChildren.map(x => x.minimax(isMaximizingPlayer, depth -1))
        allChildrenScores.reduceLeft((x,y) => if(x._2 > y._2) (this, x._2) else (this, y._2))
      }
    }
  }
  def nextStateAB(isMaximizingPlayer : Boolean, depth:Int):ChessBoardState = {
    val allChildren = playerOnePositions.flatMap(p => generateStatesForPosition(p))
    val allChildrenScores = allChildren.map(x => x.alphabeta(isMaximizingPlayer, depth -1, -2000,2000))
    if (isMaximizingPlayer) {
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 > y._2) x else y)
      bestStateTuple._1
    }else{
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 < y._2) x else y)
      bestStateTuple._1
    }
  }
  def alphabeta(isMaximizingPlayer : Boolean, depth:Int, a:Int, b:Int) : (ChessBoardState, Int) = {
    if (depth<=0 || isGameOver) {
      (this, countScoreForChessState(isMaximizingPlayer))
    }
    else {
      if (isMaximizingPlayer) {
        take_min(playerOnePositions.flatMap(p => generateStatesForPosition(p)), depth, a, b, 2000)
      }
      else {
        take_max(playerOnePositions.flatMap(p => generateStatesForPosition(p)), depth, a, b, -2000)
      }
    }
  }

  def take_max(children:Set[ChessBoardState], depth:Int, a:Int, b:Int, v_val:Int): (ChessBoardState, Int) ={
    val v:(ChessBoardState, Int) = (this,v_val)
    val ab = children.head.alphabeta(true, depth-1, a,b)
    val new_v = if(v._2 > ab._2) v else ab
    val new_a = if(new_v._2 > a) new_v._2 else a
    if(b <= new_a || children.tail.isEmpty)
      (this,new_v._2)
    else
      take_max(children.tail, depth, new_a, b, new_v._2)
  }

  def take_min(children:Set[ChessBoardState], depth:Int, a:Int, b:Int, v_val:Int): (ChessBoardState, Int) = {
    val v:(ChessBoardState, Int) = (this,v_val)
    val ab = children.head.alphabeta(false, depth-1, a,b)
    val new_v = if(v._2 < ab._2) v else ab
    val new_b = if(new_v._2 < b) new_v._2 else b
    if(new_b <= a || children.tail.isEmpty)
      (this,new_v._2)
    else
      take_min(children.tail, depth, a, new_b, new_v._2)

  }


//  def alphaBeta(isMaximizingPlayer:Boolean, depth:Int, alpha:Int, beta:Int) : (ChessBoardState, Int) = {
//    if(depth ==0 || isGameOver){
//      (this,countScoreForChessState(isMaximizingPlayer))
//    }
//    if(isMaximizingPlayer){
//
//    }
//
//  }
}

