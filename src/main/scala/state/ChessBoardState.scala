package state

import figure._
import position.{DisplayablePosition, Position}

class ChessBoardState(val playerOnePositions : Set[Position],
                      val playerTwoPositions : Set[Position],
                      val availablePositions : Set[Position]) {
  final val DIMENSION = 8

  lazy val isGameOver: Boolean = playerOneLose || playerTwoLose
  lazy val playerOneLose: Boolean = checkLose(playerOnePositions)
  lazy val playerTwoLose: Boolean = checkLose(playerTwoPositions)

  def equals(obj: ChessBoardState): Boolean = {
    this.playerOnePositions==obj.playerOnePositions && this.playerTwoPositions==obj.playerTwoPositions && this.availablePositions==obj.availablePositions
  }

  def checkLose(positions: Set[Position]): Boolean = !positions.toStream.exists(p => p.field.getClass==classOf[King])

  def makeMove(p1: Position, p2: Position): ChessBoardState = {
      val p = playerTwoPositions.toStream.find(p => p.equalCoords(p2)).orNull
      if (p != null) {
        new ChessBoardState(reversePositions(playerTwoPositions - p),reversePositions(playerOnePositions - p1 + p2), reversePositions(availablePositions + Position(p1.row, p1.col, new EmptyField())))
      }
      else {
        new ChessBoardState(reversePositions(playerTwoPositions),reversePositions(playerOnePositions - p1 + p2), reversePositions(availablePositions + Position(p1.row, p1.col, new EmptyField()) - p2))
      }
  }

  def isValidMove(p1: Position, p2: Position): Boolean = {
      val possibleStates = generateStatesForPosition(p1)
      val potentialNextState = makeMove(p1,p2)
      val x = possibleStates.map(f => f.equals(potentialNextState))
      x contains true
  }

    def display(playedComputer:Boolean): Unit = {
      println("*****************************")
      if(playedComputer){
        val chessBoard = availablePositions.map(p => DisplayablePosition(p,true)) ++ playerTwoPositions.map(p => DisplayablePosition(p,true)) ++ playerOnePositions.map(p => DisplayablePosition(p,false))
        val chessBoardSort = collection.immutable.SortedSet[DisplayablePosition]() ++ chessBoard
        chessBoardSort.grouped(8).foreach(x => {x.foreach(y=>print(y.getDisplayedValue +  " ")); println("")})
      }
      else{
        val chessBoard = availablePositions.map(p => DisplayablePosition(p,true)) ++ playerTwoPositions.map(p => DisplayablePosition(p,false)) ++ playerOnePositions.map(p => DisplayablePosition(p,true))
        val chessBoardSort = collection.immutable.SortedSet[DisplayablePosition]() ++ chessBoard
        chessBoardSort.grouped(8).foreach(x => {x.foreach(y=>print(y.getDisplayedValue +  " ")); println("")})
      }
    }



    def reversePositions(positions: Set[Position]) : Set[Position] = {
      positions.map(p => Position(7-p.row, 7-p.col, p.field))
    }

    def reverseChessBoardState() : ChessBoardState = {
      val playerOneNewPositions = reversePositions(playerTwoPositions)
      val playerTwoNewPositions = reversePositions(playerOnePositions)
      val newAvailablePositions = reversePositions(availablePositions)
      new ChessBoardState(playerOneNewPositions, playerTwoNewPositions, newAvailablePositions)
    }

  def generateStatesForPosition(position: Position) : Set[ChessBoardState] = position.field match {
    case figure : Figure => figure.generatePositions(position,this).map( p => makeMove(position, p))
    case _ : Field => throw new RuntimeException("Can't generate positions for field")

  }

  def countScoreForFigure(field : Field): Int = field match {
    case figure :Figure => figure.value
    case field : Field => throw new RuntimeException("Can't calculate score for field")
  }


  def countScoreForChessState(isMaximizingPlayer: Boolean) : Int = {
    if(isMaximizingPlayer){
      playerTwoPositions.toStream.map(p=>countScoreForFigure(p.field)).sum - playerOnePositions.toStream.map(p=>countScoreForFigure(p.field)).sum
    }
    else{
      playerOnePositions.toStream.map(p=>countScoreForFigure(p.field)).sum - playerTwoPositions.toStream.map(p=>countScoreForFigure(p.field)).sum
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

  def takeMax(children:Set[ChessBoardState], depth:Int, alpha:Int, beta:Int, valueV:Int): (ChessBoardState, Int) ={
    val v:(ChessBoardState, Int) = (this,valueV)
    val ab = children.head.alphaBeta(isMaximizingPlayer = true, depth-1, alpha,beta)
    val newV = if(v._2 > ab._2) v else ab
    val newA = if(newV._2 > alpha) newV._2 else alpha
    if(beta <= newA || children.tail.isEmpty)
      (this,newV._2)
    else
      takeMax(children.tail, depth, newA, beta, newV._2)
  }

  def takeMin(children:Set[ChessBoardState], depth:Int, alpha:Int, beta:Int, valueV:Int): (ChessBoardState, Int) = {
    val v:(ChessBoardState, Int) = (this,valueV)
    val ab = children.head.alphaBeta(isMaximizingPlayer = false, depth-1, alpha,beta)
    val newV = if(v._2 < ab._2) v else ab
    val newB = if(newV._2 < beta) newV._2 else beta
    if(newB <= alpha || children.tail.isEmpty)
      (this,newV._2)
    else
      takeMin(children.tail, depth, alpha, newB, newV._2)

  }

}

