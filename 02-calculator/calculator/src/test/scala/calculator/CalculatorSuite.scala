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

  def pDelta(a:Double, b:Double, c:Double): Double = Polynomial.computeDelta(Var(a), Var(b), Var(c))()

  def pCompute(a:Double, b:Double, c:Double): Set[Double] = {
    val aVar = Var(a)
    val bVar = Var(b)
    val cVar = Var(c)
    Polynomial.computeSolutions(aVar, bVar, cVar, Polynomial.computeDelta(aVar, bVar, cVar))()
  }

  def rootsCount(a:Double, b:Double, c:Double): Int = {
    delta(a,b,c) match {
      case aux if aux < 0 => 0
      case aux if aux == 0.0 => 1
      case _ => 2
    }
  }

  def delta(a:Double, b:Double, c:Double): Double = b*b - 4*a*c
  def root0(a:Double, b:Double, c:Double): Double = (-b + Math.sqrt(delta(a,b,c)) ) / (if (a!=0) 2*a else 1)
  def root1(a:Double, b:Double, c:Double): Double = (-b - Math.sqrt(delta(a,b,c)) ) / (if (a!=0) 2*a else 1)

  def testRoots(a:Double, b:Double, c:Double): Unit = {
    println(s"testing roots for: A=$a, B=$b, C=$c")

    println(s"   -> testing delta")
    assert( delta(a,b,c) === pDelta(a,b,c) )

    val expectedRootsCount = rootsCount(a,b,c)
    val roots = pCompute(a,b,c)
    println(s"   -> testing roots count")
    assert(expectedRootsCount === roots.size)

    val expectedRoots = expectedRootsCount match {
      case 0 => Set()
      case 1 => Set(root0(a,b,c))
      case _ => Set(root0(a,b,c), root1(a,b,c))
    }
    println(s"   -> testing roots values")
    assert( expectedRoots === roots)
  }

  test("polinomios-01") { testRoots(1,0,0) }
  test("polinomios-02") { testRoots(2,0,0) }
  test("polinomios-03") { testRoots(0,0,0) }
  test("polinomios-04") { testRoots(1,0,1) }
  test("polinomios-05") { testRoots(1,0,-1) }

  def severalTests(fromVal:Double, toVal:Double, stepVal:Double): Unit =
    for(a <- fromVal to toVal by stepVal; b <- fromVal to toVal by stepVal; c <- fromVal to toVal by stepVal)
      testRoots(a,b,c)

//  test("polinomios-varios-1") { severalTests(-20, 20, 1) }
//  test("polinomios-varios-2") { severalTests(-10, 10, 0.5) }
//  test("polinomios-varios-3") { severalTests(-5, 5, 0.25) }

  test("calculator - ok"){
    val expr1:Signal[Expr] = Var(Literal(1))
    val expr2:Signal[Expr] = Var(Literal(2))
    val expr3:Signal[Expr] = Var(Plus(expr1(), expr2()))
    val expr4:Signal[Expr] = Var(Plus(Ref("2"), Ref("3")))

    val results = Calculator.computeValues(Map("1" -> expr1, "2" -> expr2, "3" -> expr3, "4" -> expr4))
    results foreach (pair => println( pair._1 + " --> " + pair._2()))

    val expectedResults = Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 5)
    val mapedResults = results mapValues (a => a())
    assert( expectedResults === mapedResults )
  }

  test("calculator - bad reference"){
    val expr1:Signal[Expr] = Var(Literal(1))
    val expr2:Signal[Expr] = Var(Ref("3"))

    val results = Calculator.computeValues(Map("1" -> expr1, "2" -> expr2))
    results foreach (pair => println( pair._1 + " --> " + pair._2()))
    assert( 1 === results("1")() )
    assert( results("2")().isNaN )
  }

  test("calculator - circular reference"){
    val expr1:Signal[Expr] = Var(Ref("2"))
    val expr2:Signal[Expr] = Var(Ref("1"))

    val results = Calculator.computeValues(Map("1" -> expr1, "2" -> expr2))
    results foreach (pair => println( pair._1 + " --> " + pair._2()))

    assert( results("1")().isNaN )
    assert( results("2")().isNaN )
  }

  test("calculator - circular reference 2"){
    val expr1:Signal[Expr] = Var(Ref("2"))
    val expr2:Signal[Expr] = Var(Ref("3"))
    val expr3:Signal[Expr] = Var(Ref("1"))

    val results = Calculator.computeValues(Map("1" -> expr1, "2" -> expr2, "3" -> expr3))
    results foreach (pair => println( pair._1 + " --> " + pair._2()))

    assert( results("1")().isNaN )
    assert( results("2")().isNaN )
    assert( results("3")().isNaN )
  }

  test("calculator - ok2"){
    val expr1:Signal[Expr] = Var(Literal(1))
    val expr2:Signal[Expr] = Var(Literal(2))
    val expr3:Signal[Expr] = Var(Literal(3))
    val expr4:Signal[Expr] = Var(Literal(4))

    val exprPlus:Signal[Expr] = Var(Plus(Plus(expr1(), expr2()), Plus(expr3(), expr4())))
    val exprMinus:Signal[Expr] = Var(Minus(Minus(expr4(), expr3()), Minus(expr2(), expr1())))
    val exprTimes:Signal[Expr] = Var(Times(Times(expr1(), expr2()), Times(expr3(), expr4())))
    val exprDivide:Signal[Expr] = Var(Plus(Divide(expr4(), expr2()), Divide(expr3(), expr1())))

    val exprRef:Signal[Expr] = Var(Plus(Plus(Ref("exprPlus"), Ref("4")), Plus(Ref("exprTimes"), Ref("exprDivide"))))

    val results = Calculator.computeValues(Map(
      "1" -> expr1,
      "2" -> expr2,
      "3" -> expr3,
      "4" -> expr4,
      "exprPlus" -> exprPlus,
      "exprMinus" -> exprMinus,
      "exprTimes" -> exprTimes,
      "exprDivide" -> exprDivide,
      "exprRef" -> exprRef
    ))

    val expectedResults = Map(
      "1" -> 1,
      "2" -> 2,
      "3" -> 3,
      "4" -> 4,
      "exprPlus" -> 10,
      "exprMinus" -> 0,
      "exprTimes" -> 24,
      "exprDivide" -> 5,
      "exprRef" -> 43)

    val mapedResults = results mapValues (a => a())
    assert( expectedResults === mapedResults )
  }


}
