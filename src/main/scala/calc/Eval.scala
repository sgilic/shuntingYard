package calc

/*
This module evaluates the AST (expr) and returns Double result
 */
object Eval {
  def eval(e: Expr): Double = {
    e match {
      case Number(x) => x
      case BinaryOp(Mul, x, y) => eval(x) * eval(y)
      case BinaryOp(Div, x, y) => eval(x) / eval(y)
      case BinaryOp(Plus, x, y) => eval(x) + eval(y)
      case BinaryOp(Minus, x, y) => eval(x) - eval(y)
      case BinaryOp(Power, x, y) => Math.pow(eval(x), eval(y))
    }
  }
}
