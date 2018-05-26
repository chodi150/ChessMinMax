package figure

import position.Position
import state.ChessBoardState

/**
  * Created by Piotr on 25.05.2018.
  */
class Horse extends Figure{

  override val value: Int = 3
  override val displayValue: Int = 2

  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    val horseMove1 = Position(position.row + 2, position.col + 1, position.field)
    val horseMove2 = Position(position.row + 2, position.col - 1, position.field)
    val horseMove3 = Position(position.row - 2, position.col + 1, position.field)
    val horseMove4 = Position(position.row - 2, position.col - 1, position.field)
    val horseMove5 = Position(position.row + 1, position.col + 2, position.field)
    val horseMove6 = Position(position.row + 1, position.col - 2, position.field)
    val horseMove7 = Position(position.row - 1, position.col + 2, position.field)
    val horseMove8 = Position(position.row - 1, position.col - 2, position.field)
    val pseudoPossibleMoves = Set(horseMove1, horseMove2, horseMove3, horseMove4, horseMove5, horseMove6, horseMove6, horseMove7, horseMove8)
    val possibleMoves = pseudoPossibleMoves.toStream.filter(p => chessBoardState.availablePositions(p) || chessBoardState.playerTwoPositions(p)).toSet
    possibleMoves
  }


}
