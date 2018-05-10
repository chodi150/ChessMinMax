import Main.{chessBoardState1, initAvailablePositions, initPlayerOnePositions, initPlayerTwoPositions}
import org.scalatest.FunSuite

class ChessBoardStateTest extends FunSuite {

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

}
