package calc

import scala.collection.mutable

/*
This module responsibility is to parse the input string and generate the AST
 */
object Parse {

  private def addNode(op: Op, stack: mutable.Stack[Expr]): Unit = {
    assert(stack.nonEmpty, "Expression stack shouldn't be empty")
    val rightSon = stack.pop()
    assert(stack.nonEmpty, "Expression stack shouldn't be empty")
    val leftSon = stack.pop()
    stack.push(BinaryOp(op, leftSon, rightSon))
  }

  def isRParens(o: Char): Boolean = o == ')'

  def parseOp(c: Char): Option[Op] = {
    c match {
      case '+' => Option(Plus)
      case '-' => Option(Minus)
      case '*' => Option(Mul)
      case '/' => Option(Div)
      case '^' => Option(Power)
      case _ => None
    }
  }

  def parse(str: String): Expr = {
    val exprStack: mutable.Stack[Expr] = new mutable.Stack[Expr]()
    val opStack: mutable.Stack[Option[Op]] = new mutable.Stack[Option[Op]]()

    def precedenceLoop(o1: Op): Unit = {
      while (opStack.nonEmpty) {
        val o2: Op = opStack.top.getOrElse(return ())
        if (o1.isLeftAssoc() && (o1.precedence() <= o2.precedence())) {
          opStack.pop()
          addNode(o2, exprStack)
        }
        else return ()
      }
    }

    def handleChar(c: Char): Unit = {
      c match {
        case ' ' => ()
        case d if d.isDigit => exprStack.push(Number(d.asDigit))
        case '(' => opStack.push(None)
        case d if isRParens(d) =>
          while (opStack.top.isDefined) {
            addNode(opStack.pop().get, exprStack)
          }
        case _ =>
          val o1: Op = parseOp(c).get
          precedenceLoop(o1)
          opStack.push(parseOp(c))
      }
    }

    str.foreach(i => handleChar(i))
    val finalOpStack: mutable.Stack[Op] = opStack.flatten
    while (finalOpStack.nonEmpty) {
      addNode(finalOpStack.pop(), exprStack)
    }
    assert(exprStack.nonEmpty, "Something went wrong, exprStack should not be empty")
    exprStack.pop()
  }

}
