import co.scalamde.StateExample
import org.scalatest.{FlatSpec, Matchers}

class PostfixNotationTestSuite extends FlatSpec with Matchers{

  "Postfix evaluation" should "pass a basic smoke test" in {
    assert(true)
  }

  it should "convert an expression to the infix form" in {
    val expr1 = for {
      _ <- StateExample.evalOne("1")
      _ <- StateExample.evalOne("2")
      ans <- StateExample.evalOne("+")
    }
      yield ans

    expr1.runA(Nil).value shouldBe 3

    val expr2 = for {
      _ <- StateExample.evalOne("1")
      _ <- StateExample.evalOne("2")
      _ <- StateExample.evalOne("+")
      _ <- StateExample.evalOne("3")
      ans <- StateExample.evalOne("*")
    }
      yield ans

    expr2.runA(Nil).value shouldBe 9
  }

  it should "Throw an error in case of an unbalanced supplied expression" in {
    val expr = for {
      _ <- StateExample.evalOne("1")
      ans <- StateExample.evalOne("*")
    }
      yield ans

    intercept[RuntimeException]{
      expr.run(Nil).value
    }
  }

}
