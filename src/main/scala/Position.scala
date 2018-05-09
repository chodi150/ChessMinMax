
case class Position(row: Int, col: Int, figure: Int) extends Ordered[Position]{
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

  override def hashCode(): Int = {
    (row,col).hashCode()
  }
}

