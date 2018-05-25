
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
        new ChessBoardState(reversePositions(playerTwoPositions - p),reversePositions(playerOnePositions - p1 + p2), reversePositions(availablePositions + Position(p1.row, p1.col, 0)))
      }
      else {
        new ChessBoardState(reversePositions(playerTwoPositions),reversePositions(playerOnePositions - p1 + p2), reversePositions(availablePositions + Position(p1.row, p1.col, 0) - p2))
      }
  }

    def display(playedComputer:Boolean): Unit = {
      println("*****************************")
      var chessBoard:Set[Position] = Set()
      if(playedComputer){
         chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, -p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,p.figure*10))
      }
      else{
         chessBoard = availablePositions.map(p => Position(p.row,p.col,p.figure+99)) ++ playerTwoPositions.map(p => Position(p.row, p.col, 10*p.figure)) ++ playerOnePositions.map(p => Position(p.row,p.col,-p.figure))
      }
      val chessBoardSort = collection.immutable.SortedSet[Position]() ++ chessBoard
      chessBoardSort.grouped(8).foreach(x => {x.foreach(y=>print(y.figure+" ")); println("")})
    }



    def reversePositions(positions: Set[Position]) : Set[Position] = {
      positions.map(p => Position(7-p.row, 7-p.col, p.figure))
    }

    def reverseChessBoardState() : ChessBoardState = {
      val playerOneNewPositions = reversePositions(playerTwoPositions)
      val playerTwoNewPositions = reversePositions(playerOnePositions)
      val newAvailablePositions = reversePositions(availablePositions)
      new ChessBoardState(playerOneNewPositions, playerTwoNewPositions, newAvailablePositions)
    }

    def generateStatesForHorse(position: Position) : Set[ChessBoardState] = {
      val possibleMoves: Set[Position] = generatePositionsForHorse(position)
      possibleMoves.map(p => makeMove(position,p))
    }

  def generateStatesForPosition(position: Position) : Set[ChessBoardState] =  {
    val figure:Figure = mapFigure(position.figure)
    val possiblePositions = figure.generatePositions(position,this)
    possiblePositions.map(p => makeMove(position,p))
  }

  def mapFigure(num:Int):Figure = num match {
    case 1 => new Pawn()
    case 2 => new Horse()
    case 3 => new Bishop()
    case 4 => new Rook()
    case 5 => new Queen()
    case 6 => new King()
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
  def nextStateAlphaBeta(isMaximizingPlayer : Boolean, depth:Int):ChessBoardState = {
    val allChildren = playerOnePositions.flatMap(p => generateStatesForPosition(p))
    val allChildrenScores = allChildren.map(x => x.alphaBeta(isMaximizingPlayer, depth -1, -2000,2000))
    if (isMaximizingPlayer) {
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 > y._2) x else y)
      bestStateTuple._1
    }else{
      val bestStateTuple = allChildrenScores.reduceLeft((x, y) => if (x._2 < y._2) x else y)
      bestStateTuple._1
    }
  }
  def alphaBeta(isMaximizingPlayer : Boolean, depth:Int, a:Int, b:Int) : (ChessBoardState, Int) = {
    if (depth<=0 || isGameOver) {
      (this, countScoreForChessState(isMaximizingPlayer))
    }
    else {
      if (isMaximizingPlayer) {
        takeMin(playerOnePositions.flatMap(p => generateStatesForPosition(p)), depth, a, b, 2000)
      }
      else {
        takeMax(playerOnePositions.flatMap(p => generateStatesForPosition(p)), depth, a, b, -2000)
      }
    }
  }

  def takeMax(children:Set[ChessBoardState], depth:Int, a:Int, b:Int, v_val:Int): (ChessBoardState, Int) ={
    val v:(ChessBoardState, Int) = (this,v_val)
    val ab = children.head.alphaBeta(isMaximizingPlayer = true, depth-1, a,b)
    val newV = if(v._2 > ab._2) v else ab
    val newA = if(newV._2 > a) newV._2 else a
    if(b <= newA || children.tail.isEmpty)
      (this,newV._2)
    else
      takeMax(children.tail, depth, newA, b, newV._2)
  }

  def takeMin(children:Set[ChessBoardState], depth:Int, a:Int, b:Int, v_val:Int): (ChessBoardState, Int) = {
    val v:(ChessBoardState, Int) = (this,v_val)
    val ab = children.head.alphaBeta(isMaximizingPlayer = false, depth-1, a,b)
    val newV = if(v._2 < ab._2) v else ab
    val newB = if(newV._2 < b) newV._2 else b
    if(newB <= a || children.tail.isEmpty)
      (this,newV._2)
    else
      takeMin(children.tail, depth, a, newB, newV._2)

  }

}

