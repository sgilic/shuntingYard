import calc.{BinaryOp, Calc, Div, Minus, Mul, Number, Plus, Power}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CalcTest extends AnyFreeSpec {
  val zero = 0.0
  val one = 1.0
  val two = 2.0
  val three = 3.0
  val four = 4.0
  val six = 6.0
  val seven = 7.0
  val nine = 9.0
  "Eval test " - {
    "Calc Number" in {
      Calc.calc("1") shouldBe one
      Calc.calc(("1/0")) shouldBe Double.PositiveInfinity
    }
    "Calc simple BinaryOp" in {
      Calc.calc("1 + 1") shouldBe two
      Calc.calc(("2 - 1")) shouldBe one
      Calc.calc(("2 * 3")) shouldBe six
      Calc.calc(("6/3")) shouldBe two
      Calc.calc("2^2") shouldBe four
    }
    "Calc structure simple BinaryOps" in {
      Calc.calc("1+2-3") shouldBe zero
      Calc.calc("1+2*3") shouldBe seven
      Calc.calc("2*3+1") shouldBe seven
      Calc.calc("(1+2)*3") shouldBe nine
      Calc.calc("3+2^2") shouldBe seven
      Calc.calc("2^2+3") shouldBe seven
    }
  }
}
