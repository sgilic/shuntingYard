import calc.{BinaryOp, Calc, Div, Minus, Mul, Number, Plus, Power}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ParseTest extends AnyFreeSpec{
  val one = Number(1.0)
  val two = Number(2.0)
  val three = Number(3.0)
  val onePlusTwo = BinaryOp(Plus, one, two)
  val twoTimesThree = BinaryOp(Mul, two, three)
  "Parse test " - {
    "Parse Number" in {
      Calc.parse("1") shouldBe Number(1.0)

    }
    "Parse simple BinaryOp" in {
      Calc.parse("1 + 1") shouldBe BinaryOp(Plus, one, one)
      Calc.parse(("2 - 1")) shouldBe BinaryOp(Minus, Number(2.0),Number(1.0))
      Calc.parse(("2 * 3")) shouldBe BinaryOp(Mul, Number(2.0),Number(3.0))
      Calc.parse(("6/3")) shouldBe BinaryOp(Div, Number(6.0),Number(3.0))
      Calc.parse("2^2") shouldBe(BinaryOp(Power, two, two))
    }
    "Parse structure simple BinaryOps" in {
      Calc.parse("1+2-3") shouldBe BinaryOp(Minus, onePlusTwo, three)
      Calc.parse("1+2*3") shouldBe BinaryOp(Plus, one, twoTimesThree)
      Calc.parse("2*3+1") shouldBe BinaryOp(Plus, twoTimesThree, one)
      Calc.parse("(1+2)*3") shouldBe BinaryOp(Mul, onePlusTwo, three)
      Calc.parse("1+2^2") shouldBe(BinaryOp(Plus, one, BinaryOp(Power, two, two)))
      Calc.parse("2^2+1") shouldBe(BinaryOp(Plus, BinaryOp(Power, two, two), one))
    }
  }
}
