import co.scalamde.WriterExample
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FactorialWriterTestSuite extends FlatSpec with Matchers{

  "Factorial without Writer" should "calculate the factorial of a given number" in {
    WriterExample.factorial(5) === 120
  }

  it should "calculate multiple factorials async" in {
    Await.result(
      Future.sequence(
        Vector(
          Future(WriterExample.factorial(5)),
          Future(WriterExample.factorial(4))
        )
      ), Duration.Inf
    )

    assert(true)
  }

  it should "calculate factorialW of a given number" in {
    val writer = WriterExample.factorialW(5).run

    writer._1.foreach(println)

    writer._2 shouldBe 120
  }

  it should "calculate multiple factorialW async" in {
    val writerAsync: Vector[(Vector[String], Int)] = Await.result(
      Future.sequence(
        Vector(
          Future(WriterExample.factorialW(5).run),
          Future(WriterExample.factorialW(5).run)
        )
      ), Duration.Inf
    )

    writerAsync.foreach(_._1.foreach(println))

    assert(true)
  }
}
