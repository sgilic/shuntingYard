package calc

import scala.collection.mutable
import scala.collection.mutable.Stack

object Parse {

  private def addNode(op: Op, stack: Stack[Expr]): Unit = {
    val rightSon = stack.pop()
    val leftSon = stack.pop()
    stack.push(BinaryOp(op, leftSon, rightSon))
    //TODO verify non empty stack
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
    val exprStack: Stack[Expr] = new mutable.Stack[Expr]()
    val opStack: Stack[Option[Op]] = new mutable.Stack[Option[Op]]()

    def precedenceLoop(o1: Op) : Unit = {
      while (opStack.nonEmpty) {
        var o2: Op = opStack.top.getOrElse(return ())
        if ((o1.isLeftAssoc()) && (o1.precedence() <= o2.precedence())) {
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
        case d if isRParens(d) => {
          while (opStack.top.isDefined) {
            addNode(opStack.pop().get, exprStack)
          }
        }
        case _ => {
          val o1: Op = parseOp(c).get
          precedenceLoop(o1)
          opStack.push(parseOp(c))
        }
      }
    }

    str.foreach(i => handleChar(i))
    val finalOpStack: Stack[Op] = opStack.flatten
    while (finalOpStack.nonEmpty) {
      addNode(finalOpStack.pop(), exprStack)
    }
    exprStack.pop()
  }

}
