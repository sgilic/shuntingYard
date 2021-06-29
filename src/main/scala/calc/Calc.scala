package calc

/*
Calc is the "main" class that contains the methods exposed to the user:
parse: String -> Expr
eval: Expr -> Double
calc: String -> Double
 */
object Calc {
  def parse(str: String): Expr = {
    Parse.parse(str)
  }
  def eval(expr: Expr): Double = {
    Eval.eval(expr)
  }
  def calc(str: String): Double = {
    eval(parse(str))
  }
}

/*
definition of Expr (expression types)
Expr can be:
* A number "1" is an expression
* A BinaryOp that contains two expressions such as "1 + 2"
*/
sealed trait Expr
case class Number(value: Double) extends Expr
case class BinaryOp(op: Op, left: Expr, right: Expr) extends Expr
case class NullExpr() extends Expr

sealed trait Op {
  def isLeftAssoc(): Boolean
  def precedence(): Int
}
case object Plus extends Op {
  override def isLeftAssoc(): Boolean = true
  override def precedence(): Int = 1
}
case object Minus extends Op {
  override def isLeftAssoc(): Boolean = true
  override def precedence(): Int = 1
}
case object Mul extends Op {
  override def isLeftAssoc(): Boolean = true
  override def precedence(): Int = 2
}
case object Div extends Op {
  override def isLeftAssoc(): Boolean = true
  override def precedence(): Int = 2
}
case object Power extends Op {
  override def isLeftAssoc(): Boolean = false
  override def precedence(): Int = 3
}
