package co.scalamde

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.writer._
import cats.syntax.applicative._

/**
  * Este ejemplo fue tomado del libro: "Advanced Scala With Cats" en el cual
  * se describe una forma inicial de implementar una función recursiva para
  * calcular el factorial de un número (para aquel que no lo sepa/recuerde,
  * el factorial de un número consiste en el producto de todos los números
  * positivos desde 1 hasta el número al que se le está calculando este valor,
  * e.g. 5! = 1*2*3*4*5 = 120).
  *
  * La primera aproximación (factorial) imprime el factorial del número que
  * se está calculando en cada llamada a la función recursiva. Esto lo hace
  * a través de la stdout por lo cual, si esta se paraleliza llamándose desde
  * futuros, por ejemplo, no se garantiza el orden de los logs impresos. Esto
  * es especialmente notorio si se tiene una lista de futuros que llaman a la misma función.
  *
  * Para hacer el ejemplo más ilustrativo, en el libro definen una función (slowly)
  * la cual a su vez recibe otra función la cual es ejecutada y al final se da un
  * delay de 100 ms.
  *
  *
  * La segunda aproximación toma ventaja de Writer monad para "aislar" cada llamada
  * a la función factorialW en su propio contexto monádico. Así, cada vez que se impriman
  * cosas estos logs quedarán almacenados en un vector de Strings junto con el valor final
  * del cálculo.
  *
  * Note varias cosas en la sintáxis:
  *
  * 1. Si n = 0, retornamos 1, pero usamos una sintaxis especial de cats para levantar ese
  * valor en un tipo Fact (que es un alias de Writer[Vector[String], A]). No ahondaré mucho
  * en detalles aquí, pero pure nos permite levantar este valor en un contexto monádico específico. Esto
  * son functores aplicativos los cuales se salen del scope de este ejercicio.
  *
  * 2. En nuestro for-comprehension usamos nuevamente slowly para *ralentizar* la ejecución de cada
  * llamada a factorialW, pero a su vez al Writer que produce esta función le añadimos los logs generados
  * por cada llamada con .tell (función que ya vimos en nuestra test suite).
  *
  * 3. Al usar flatMap en Writer (for-comprehensions) como mínimo deberíamos tener dos pasos:w
  *
  * a. La ejecución de nuestro cómputo.
  * b. El "append" a los logs que generamos
  *
  */

object WriterExample {

  type Fact[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))

    println(s"Factorial of $n is $ans")

    ans
  }

  def factorialW(n: Int): Fact[Int] = {
    if (n == 0) {
      1.pure[Fact]
    } else {
      for {
        a <- slowly(factorialW(n - 1))
        _ <- Vector(s"FactorialW of $n is ${a * n}").tell
      }
        yield a * n
    }

  }

}
