package co.scalamde

import cats.data.State

/**
  * Este ejemplo fue tomado del libro "Advanced Scala with Cats" en el cual
  * se describe una aplicación en la cual se necesita guardar estado y ejecutar
  * una serie de operaciones subsecuentes sobre dicho estado.
  *
  * Este ejercicio plantea la conversión de una expresión escrita en notación
  * postfijo (por ejemplo: 1 2 +) a notación infijo (1 + 2). La manera "tradicional" de
  * lograr esto es a través de una pila (stack) + recursividad, pero dado que con State
  * tenemos un contexto monádico con el que podemos hacer operaciones que nos retornan un
  * resultado "intermedio" y adicional almacena el estado (la pila, en nuestro caso) podemos
  * tomar ventaja de esto para implementarlo de una manera funcional.
  *
  * ¿Cómo funciona?
  *
  * Primero, definimos un alias de State[List[Int], A] el cuál será nuestro contexto monádico
  * que almacenará nuestros cómputos. Hay una función que es el punto de entrada llamda "evalOne", la
  * cual espera que se le pase un operador básico (+, -, *, /) ó un operando (entero).
  *
  * evalOne depende de dos funciones "auxiliares": una para procesar un operando y una para procesar
  * un operador.
  *
  * operand es la más sencilla de todas: cada vez que ve un operando lo apila y retorna como resultado
  * parcial de la operación el mismo número que acaba de apilar.
  *
  * operator es más compleja: siempre dependemos de dos operandos en la pila para aplicar un operador.
  * Es decir, si hay un 1 y un 1 en la expresión lo siguiente que se debería ver es el operador que le
  * aplica a ambos.
  * La función hace pop de los dos operandos, evalúa estos (a :: b :: tail) y aplica la función
  * que se le pasó dependiendo del operador:
  * Suma (_+_) si el operador fue "+", resta (_-_) si el operador fue "-" y así. Con la
  * evaluación de estos operandos se hace push a la pila nuevamente y se retorna ese resultado
  * parcial.
  * Sigue así sucesivamente hasta que no haya más elementos qué evaluar. El resultado final
  * es el resultado de la última evaluación (o el del estado final, depende de cuál se quiera
  * tomar).
  *
  * Cabe aclarar que este ejercicio es una demostración, no es "fool-proof", dado que la idea
  * es simplemente demostrar el funcionamiento de State.
  */

object StateExample {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  private def operand(num: Int): CalcState[Int] = State[List[Int], Int] { stack =>
    (num :: stack, num)
  }

  private def operator(func: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case a :: b :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ => sys.error("Fail!")
  }

}
