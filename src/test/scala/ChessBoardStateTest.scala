import Main.{initAvailablePositions, initPlayerOnePositions, initPlayerTwoPositions}
import figure._
import org.scalatest.FunSuite
import position.Position
import state.ChessBoardState

class ChessBoardStateTest extends FunSuite {

  val initialStateOfGame = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
  test("testGenerateStatesForPositionForPawn1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(1,1,new Pawn())
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,1,new Pawn())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,3,new Pawn())
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,3,new Pawn())
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,new Pawn())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,0,new Pawn())
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn7") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,new Pawn())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,0,new Rook())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(4,3,new Rook())
    assert(11==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,7,new Rook())
    assert(11==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(1,3,new Rook())
    assert(5==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,new Rook())
    assert(3==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,new Rook())
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook7") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,new Rook())
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }

  test("testGenerateStatesForPositionForQueen1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,new Queen())
    assert(3==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,0,new Queen())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,3,new Queen())
    assert(19==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,3,new Queen())
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,new Queen())
    assert(5==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,7,new Queen())
    assert(15==chessBoardState.generateStatesForPosition(p).size)
  }

  test("figure.King in proper position in initial state of game") {
    val positionOfKing = initialStateOfGame.playerOnePositions.filter(p => p.field.getClass==classOf[King]).head
    assert(positionOfKing.row == 0 && positionOfKing.col == 4)
  }

  test("figure.King has no possible moves in start positions") {
    val positionOfKing = initialStateOfGame.playerOnePositions.filter(p => p.field.getClass == classOf[King]).head
    assert(0 == initialStateOfGame.generateStatesForPosition(positionOfKing).size)
  }
  test("initial position, besides figure.King staying in row 5, can beat 2 pawns or choose different 6 positions") {
    val positionOfKing = Position(5,4,new King())
    assert(8 == initialStateOfGame.generateStatesForPosition(positionOfKing).size)
  }

  test("Horses in proper positions in initial state"){
    val positonOfHorses = initialStateOfGame.playerOnePositions.filter(p => p.field.getClass == classOf[Horse])
    val properPositions = Set(Position(0,1,new Horse()), Position(0,6,new Horse()))
    assert((positonOfHorses -- properPositions).isEmpty)
  }
  test("Horses in initial states, both can go to two positions"){
    val positonOfHorses = initialStateOfGame.playerOnePositions.filter(p => p.field.getClass == classOf[Horse])
    val moves = positonOfHorses.map(x => initialStateOfGame.generateStatesForPosition(x)).toList
    assert(moves(0).size ==2)
    assert(moves(1).size ==2 )
  }

  test("Bishops can't move in initial state") {
    val positonOfBishop = initialStateOfGame.playerOnePositions.filter(p => p.field.getClass == classOf[Bishop])
    val moves = positonOfBishop.flatMap(x => initialStateOfGame.generateStatesForPosition(x))
    assert(moves.isEmpty)
  }
  test("figure.Bishop in the middle can move to 10 fields") {
    val positionOfBishop = Position(4,4,new Bishop())
    val moves = initialStateOfGame.generateStatesForPosition(positionOfBishop)
    assert(moves.size == 8)
  }

  test("Initial state has score of 0") {
    assert(initialStateOfGame.countScoreForChessState(true) == 0)
  }

  test("Initial state has score of 0 2") {
    assert(initialStateOfGame.countScoreForChessState(false) == 0)
  }

  test("makeMove test1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,3,new Queen())
  }

  test("figure.Queen and king in danger, CPU saves king"){
    val state1 = initialStateOfGame.makeMove(Position(0,1,new Horse()), Position(5,2,new Horse()))
    val state2 = state1.makeMove(Position(1,1,new Pawn()), Position(1,1,new Pawn()))
    val state3 = state2.makeMove(Position(0,6,new Horse()), Position(5,5,new Horse()))
    val state4 = state3.nextState(true, 4)
    assert(state4.playerTwoPositions(Position(5,5,new Pawn())))
  }
  test("depth 1 - if is able to smash then does it 0") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val chessBoardState2 = chessBoardState.makeMove(Position(1,0,new Pawn()), Position(5,0,new Pawn()))
    val chessBoardState3 = chessBoardState2.nextState(true,1)
    assert(!chessBoardState3.playerOnePositions.contains(Position(5,0,new Pawn())))
  }
  test("depth 1 - if is able to smash then does it 1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val chessBoardState2 = chessBoardState.makeMove(Position(1,1,new Pawn()), Position(5,1,new Pawn()))
    val chessBoardState3 = chessBoardState2.nextState(true,1)
    assert(!chessBoardState3.playerOnePositions.contains(Position(5,1,new Pawn())))
  }

  test("figure.Queen and king in danger, CPU saves king ALPHA BETTTTA"){
    val state1 = initialStateOfGame.makeMove(Position(0,1,new Horse()), Position(5,2,new Horse()))
    val state2 = state1.makeMove(Position(1,1,new Pawn()), Position(1,1,new Pawn()))
    val state3 = state2.makeMove(Position(0,6,new Horse()), Position(5,5,new Horse()))
    val state4 = state3.nextStateAlphaBeta(true, 4)
    assert(state4.playerTwoPositions(Position(5,5,new Pawn())))
  }
  test("depth 1 - if is able to smash then does it 0 ALPHA BETTTTA") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val chessBoardState2 = chessBoardState.makeMove(Position(1,0,new Pawn()), Position(5,0,new Pawn()))
    val chessBoardState3 = chessBoardState2.nextStateAlphaBeta(true,1)
    assert(!chessBoardState3.playerOnePositions.contains(Position(5,0,new Pawn())))
  }
  test("depth 1 - if is able to smash then does it 1 ALPHA BETTTTA") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val chessBoardState2 = chessBoardState.makeMove(Position(1,1,new Pawn()), Position(5,1,new Pawn()))
    val chessBoardState3 = chessBoardState2.nextStateAlphaBeta(true,1)
    assert(!chessBoardState3.playerOnePositions.contains(Position(5,1,new Pawn())))
  }

  test("depth4 AB - cpu's king is under attack, only one move can save him ") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(), initPlayerTwoPositions(), initAvailablePositions());
    val state2 = chessBoardState.makeMove(Position(0, 2,new Bishop()), Position(4, 7,new Bishop()))
    val state3 = state2.makeMove(Position(1, 2,new Pawn()), Position(2, 2,new Pawn()))
    val state4 = state3.makeMove(Position(1, 1,new Pawn()), Position(2, 1,new Pawn()))
    val state5 = state4.nextStateAlphaBeta(true, 4)
    assert(state5.playerTwoPositions.contains(Position(5, 6,new Pawn())))
  }

    test("depth4 minimax - cpu's king is under attack, only one move can save him "){
      val chessBoardState = new ChessBoardState(initPlayerOnePositions(), initPlayerTwoPositions(), initAvailablePositions());
      val state2 = chessBoardState.makeMove(Position(0,2,new Bishop()), Position(4,7,new Bishop()))
      val state3 = state2.makeMove(Position(1,2,new Pawn()), Position(2,2,new Pawn()))
      val state4 = state3.makeMove(Position(1,1,new Pawn()), Position(2,1,new Pawn()))
      val state5 = state4.nextState(true,4)
      assert(state5.playerTwoPositions.contains(Position(5,6,new Pawn())))
    }

  test("depth2 AB - cpu's king is under attack, only one move can save him ") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(), initPlayerTwoPositions(), initAvailablePositions());
    val state2 = chessBoardState.makeMove(Position(0, 2,new Bishop()), Position(4, 7,new Bishop()))
    val state3 = state2.makeMove(Position(1, 2,new Pawn()), Position(2, 2,new Pawn()))
    val state4 = state3.makeMove(Position(1, 1,new Pawn()), Position(2, 1,new Pawn()))
    val state5 = state4.nextStateAlphaBeta(true, 2)
    assert(state5.playerTwoPositions.contains(Position(5, 6,new Pawn())))
  }

  test("depth2 minimax - cpu's king is under attack, only one move can save him "){
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(), initPlayerTwoPositions(), initAvailablePositions());
    val state2 = chessBoardState.makeMove(Position(0,2,new Bishop()), Position(4,7,new Bishop()))
    val state3 = state2.makeMove(Position(1,2,new Pawn()), Position(2,2,new Pawn()))
    val state4 = state3.makeMove(Position(1,1,new Pawn()), Position(2,1,new Pawn()))
    val state5 = state4.nextState(true,2)
    assert(state5.playerTwoPositions.contains(Position(5,6,new Pawn())))
  }


  test("CPU has king in danger, but can beat opponents KING, he decides to attack ! ALFA BETA"){
    val state0 = initialStateOfGame.makeMove(Position(1,1,new Pawn()), Position(1,1,new Pawn()))
    val state1 = state0.makeMove(Position(0,4,new Queen()), Position(6,3,new Queen()))
    val state2 = state1.makeMove(Position(0,3,new Queen()), Position(6,4,new Queen()))
    val state3 = state2.nextStateAlphaBeta(true,1)
    assert(state3.playerTwoPositions.contains(Position(0,4,new Queen())))
  }

  test("CPU has king in danger, but can beat opponents KING, he decides to attack ! minmax"){
    val state0 = initialStateOfGame.makeMove(Position(1,1,new Pawn()), Position(1,1,new Pawn()))
    val state1 = state0.makeMove(Position(0,4,new Queen()), Position(6,3,new Queen()))
    val state2 = state1.makeMove(Position(0,3,new Queen()), Position(6,4,new Queen()))
    val state3 = state2.nextState(true,1)
    assert(state3.playerTwoPositions.contains(Position(0,4,new Queen())))
  }

}
