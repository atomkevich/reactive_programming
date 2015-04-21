package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }
  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeSolution have 2 solutions") {
    val delta = Polynomial.computeDelta(Var(1), Var(-8), Var(12))
    val kor = Polynomial.computeSolutions(Var(1), Var(-8), Var(12), delta)
    assert(kor().size ==2)
  }

  test("computeSolution haven't solutions") {
    val delta = Polynomial.computeDelta(Var(5), Var(3), Var(7))
    val kor = Polynomial.computeSolutions(Var(5), Var(3), Var(7), delta)
    assert(kor().size == 0)
  }

  test("computeSolution have 1 solution") {
    val delta = Polynomial.computeDelta(Var(1), Var(-6), Var(9))
    val kor = Polynomial.computeSolutions(Var(1), Var(-6), Var(9), delta)
    assert(kor().size == 1)
  }

  test("calculator test for literal expressions") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a1" -> Var(Literal(1)))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("a1").get() == 1)
  }

  test("calculator test for ref expressions") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(1)), "b" -> Var(Ref("a")))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("b").get() == 1)
  }

  test("calculator test for sum expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(2)),
                                                          "b" -> Var(Ref("a")),
                                                           "c" -> Var(Plus(Ref("a"), Ref("b"))))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("c").get() == 4)
  }

  test("calculator test for minus expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(2)),
      "b" -> Var(Ref("a")),
      "c" -> Var(Minus(Ref("a"), Ref("b"))))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("c").get() == 0)
  }

  test("calculator test for times expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(2)),
      "b" -> Var(Ref("a")),
      "c" -> Var(Times(Ref("a"), Ref("b"))))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("c").get() == 4)
  }

  test("calculator test for devide expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(2)),
      "b" -> Var(Ref("a")),
      "c" -> Var(Divide(Ref("a"), Ref("b"))))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    assert(result.get("c").get() == 1)
  }
}
