package lang

abstract class TableConstraint

object TableConstraint {
  /** Constraint that ensures that all values of column `col` are unique */
  case class Unique(col: TokenId) extends TableConstraint
  /** Not really a constraint, but makes two columns unordered in respect to each other */
  case class Unordered(col0: TokenId, col1: TokenId) extends TableConstraint
  /** Spcecify a default value `value` for a column `col` */
  case class Default(col: TokenId, value: AstEx) extends TableConstraint
}

class Table(val columns: Vector[String], val constraints: Vector[TableConstraint]) {



}

