import calc.{BinaryOp, Calc, Div, Number, Plus, Minus, Mul}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AstEvalTest extends AnyFreeSpec{

  "AST eval test " - {
    "MyTest" in{
      val just3 = Number(3)
      val just1 = Number(1)
//      Calc.eval(just1) shouldBe 1.0
      Calc.eval(just3) shouldBe 3.0
      val just0 = Number(0)
      val just6 = Number(6)
      val div_3_6 = BinaryOp(Div,just3,just6)
      Calc.eval(div_3_6) shouldBe 0.5
      val div_3_0 = BinaryOp(Div, just3, just0)
      Calc.eval(div_3_0).isInfinity shouldBe true
      Calc.eval(BinaryOp(Plus, Number(4), (BinaryOp(Div, Number(18),(BinaryOp(Minus, Number(9), Number(3))))))) shouldBe 7.0
    }
  }
}
