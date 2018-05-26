package figure

import position.Position
import state.ChessBoardState

/**
  * Created by Piotr on 25.05.2018.
  */
class Pawn extends Figure{
  override val value: Int = 1
  override val displayValue: Int = 1
  override def generatePositions(position: Position, chessBoardState: ChessBoardState): Set[Position] = {
    val emptyFields:Set[Position] = Set.empty ++ chessBoardState.availablePositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col, position.field))).toSet ++ chessBoardState.playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col+1, position.field))).toSet ++ chessBoardState.playerTwoPositions.toStream.find(p => p.equalCoords(Position(position.row + 1, position.col-1, position.field))).toSet
    emptyFields.map( x => Position(x.row, x.col, new Pawn()))
  }
}
