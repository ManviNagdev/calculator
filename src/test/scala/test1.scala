import org.scalatest.funsuite.AnyFunSuite
import manvi.Calculator

class TestCalculator extends AnyFunSuite {

  def makeAnswer(answer: Any): String = {
    answer match {
      case answer: Double => answer.toString
      case answer: Int    => makeAnswer(answer.toDouble)
      case answer: String => answer
      case default        => throw new Exception("unrecognized type")
    }
  }

  test("Test 1") {
    assert(Calculator.calculate("1") == makeAnswer(1))
  }
  test("Test 2") {
    assert(Calculator.calculate("1-2-3") == makeAnswer(-4))
  }
  test("Test 3") {
    assert(Calculator.calculate("16.8/4/2") == makeAnswer(2.1))
  }
  test("Test 4") {
    assert(Calculator.calculate("1+2+12/4-3*3-2-1") == makeAnswer(-6))
  }
  test("Test 5") {
    assert(Calculator.calculate("(10-(2+2)*2)/2") == makeAnswer(1))
  }
  test("Test 6") {
    assert(Calculator.calculate("10-(2+2*2)/2") == makeAnswer(7))
  }
  test("Test 7") {
    assert(Calculator.calculate("((-2+2))*2") == makeAnswer(0))
  }
  test("Test 8") {
    assert(Calculator.calculate("-0.32     /0.5") == makeAnswer(-0.64))
  }
  test("Test 9") {
    assert(Calculator.calculate("-5+ -8--11*2") == makeAnswer(9))
  }
  test("Test 10") {
    assert(
      Calculator.calculate("3+-+2") == makeAnswer("invalid expression format")
    )
  }
}
