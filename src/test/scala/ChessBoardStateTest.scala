import Main.{initAvailablePositions, initPlayerOnePositions, initPlayerTwoPositions}
import Main.{chessBoardState1, initAvailablePositions, initPlayerOnePositions, initPlayerTwoPositions}
import org.scalatest.FunSuite

class ChessBoardStateTest extends FunSuite {

  val initialStateOfGame = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
  test("testGenerateStatesForPositionForPawn1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(1,1,1)
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,1,1)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,3,1)
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,3,1)
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,1)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,0,1)
    assert(1==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForPawn7") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,1)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,0,4)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(4,3,4)
    assert(11==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(5,7,4)
    assert(11==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(1,3,4)
    assert(5==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,4)
    assert(3==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,4)
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForRook7") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,4)
    assert(2==chessBoardState.generateStatesForPosition(p).size)
  }

  test("testGenerateStatesForPositionForQueen1") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,7,5)
    assert(3==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen2") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,0,5)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen3") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,3,5)
    assert(19==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen4") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(0,3,5)
    assert(0==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen5") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(7,3,5)
    assert(5==chessBoardState.generateStatesForPosition(p).size)
  }
  test("testGenerateStatesForPositionForQueen6") {
    val chessBoardState = new ChessBoardState(initPlayerOnePositions(),initPlayerTwoPositions(),initAvailablePositions())
    val p = Position(3,7,5)
    assert(15==chessBoardState.generateStatesForPosition(p).size)
  }

  test("King in proper position in initial state of game") {
    val positionOfKing = initialStateOfGame.playerOnePositions.filter(p => p.figure == 6).head
    assert(positionOfKing.row == 0 && positionOfKing.col == 4)
  }

  test("King has no possible moves in start positions") {
    val positionOfKing = initialStateOfGame.playerOnePositions.filter(p => p.figure == 6).head
    assert(0 == initialStateOfGame.generateStatesForKing(positionOfKing).size)
  }
  test("initial position, besides King staying in row 5, can beat 2 pawns or choose different 6 positions") {
    val positionOfKing = Position(5,4,6)
    assert(8 == initialStateOfGame.generateStatesForKing(positionOfKing).size)
  }

  test("initial position, besides King staying in row 4, col 7, has 5 moves (all - 3 out of board)") {
    val positionOfKing = Position(5,7,6)
    val correctPossibilities = Set.empty + Position(6,7,6) + Position(6,6,6) + Position(5,6,6) + Position(4,6,6) + Position(4,7,6)
    assert(5 == initialStateOfGame.generateStatesForKing(positionOfKing).size)
    assert((initialStateOfGame.generatePositionsForKing(positionOfKing) -- correctPossibilities).isEmpty )
  }

  test("Horses in proper positions in initial state"){
    val positonOfHorses = initialStateOfGame.playerOnePositions.filter(p => p.figure == 2)
    val properPositions = Set(Position(0,1,2), Position(0,6,2))
    assert((positonOfHorses -- properPositions).isEmpty)
  }
  test("Horses in initial states, both can go to two positions"){
    val positonOfHorses = initialStateOfGame.playerOnePositions.filter(p => p.figure == 2)
    val moves = positonOfHorses.map(x => initialStateOfGame.generateStatesForHorse(x)).toList
    assert(moves(0).size ==2)
    assert(moves(1).size ==2 )
  }
  test("Initial state besides horse in row 5, col 7, has 3 moves"){
    val moves = initialStateOfGame.generatePositionsForHorse(Position(5,7,2))
    val properPositions = Set(Position(7,6,2), Position(6,5,2), Position(4,5,2), Position(3,6,2))
    assert((moves--properPositions).isEmpty)
  }
  test("Bishops can't move in initial state") {
    val positonOfBishop = initialStateOfGame.playerOnePositions.filter(p => p.figure ==3)
    val moves = positonOfBishop.flatMap(x => initialStateOfGame.generateStatesForBishop(x))
    assert(moves.isEmpty)
  }
  test("Bishop in the middle can move to 10 fields") {
    val positionOfBishop = Position(4,4,3)
    val moves = initialStateOfGame.generateStatesForBishop(positionOfBishop)
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
    val p = Position(0,3,5)
  }

}
