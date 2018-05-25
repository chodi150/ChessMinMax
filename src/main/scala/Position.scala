
case class Position(row: Int, col: Int, field: Field) extends Ordered[Position]{
  override def compare(that: Position): Int = {
    if (this.col==that.col && this.row==that.row)
      0
    else if(this.row>that.row)
      1
    else if(this.row==that.row && this.col>that.col)
      1
    else
      -1
  }

  override def equals(obj: scala.Any): Boolean = this.hashCode() == obj.hashCode()

  override def hashCode(): Int = (row,col).hashCode()

  def equalCoords(position: Position): Boolean = this.compare(position)==0


  def positionOnBoard():Boolean = row<8 && row>=0 && col<8 && col>=0
}

