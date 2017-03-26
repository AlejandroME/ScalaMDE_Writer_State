import org.scalatest.{FlatSpec, Matchers}
import cats.data.State

class StateTestSuite extends FlatSpec with Matchers{

  "State" should "pass a basic smoke test" in {
    assert(true)
  }

  /**
    * En el caso de esta test suite, vamos a tener una State Monad de la siguiente forma:
    *
    * State[(Int, Int), String] en dónde:
    *
    * (Int, Int): representa un par de coordenadas en una matriz. Haremos de cuenta
    * que el estado que tendremos que guardar es la posición de un objeto que va a
    * estarse moviendo conforme se le entreguen comandos.
    *
    * String: Nuestra evaluación, aparte de cambiar el estado, será notificar con
    * un mensaje cuál fue la nueva posición en la que el objeto quedó.
    *
    *
    * ¿QUÉ ES STATE MONAD?
    *
    * State nos permite guardar estado. Si en algún momento tenemos algún cómputo
    * que necesitemos mutar y este pase por una cadena de operaciones que necesiten recuperar
    * el estado anterior podremos usar state.
    * Adicional a esto State siempre en su ejecución nos va a devolver un resultado "intermedio", es
    * decir: yo ejecuto una operación y eso me va a dar un resultado que necesito, pero a su vez
    * dicho resultado podrá ser (no necesariamente es así, como por ejemplo aquí) el nuevo estado.
    *
    * En nuestro caso estamos moviendo un objeto en una matriz, algo así como un robot que se mueve
    * dadas ciertas coordenadas. Usted, cada vez que le dice al robot: "muévase a este par de coordenadas"
    * tendrá un resultado (un mensaje diciendo cuál es su posición actual) y adicional guardando la tupla
    * de coordenadas en las que quedó.
    */

  // TODO relación con Eval
  it should "Move one position" in {
    val grid = State[(Int, Int), String] {
      state =>
        (state, s"Current position ---> X: ${state._1}, Y: ${state._2}")
    }

    /**
      * Al igual que Reader y Writer, state también es lazy y su resultado se evalúa con
      * run. En este caso estamos ejecutando la función con el par de coordenadas (0,1).
      * La evaluación de esto en Cats produce un resultado monádico de tipo Eval (no profundizaré
      * en esto aquí) del cual obtenemos el valor generado por este paso del cómputo.
      */

    grid.run((0, 1)).value shouldBe ((0, 1), "Current position ---> X: 0, Y: 1")
  }

  /**
    * En State tenemos tres tipos diferentes de .run
    *
    * run: Ejecuta el cómputo y nos da acceso tanto a su resultado como al estado.
    * .runS: Ejecuta el cómputo pero sólo nos devuelve el estado.
    * .runA: Ejecuta el cómputo pero sólo nos devuelve el resultado.
    *
    * Aquí en este test veremos un ejemplo de .runS
    *
    * Fíjese en los siguientes dos tests un tema importante: a pesar que la State Monad
    * tiene la operación que usted hará y el nuevo estado en el que quedará, cuando encadenamos
    * states en un For-comprehension no interactuamos directamente con este, interactuamos es con
    * el valor inmediato que obtendremos de nuestro cómputo.
    */
  it should "Get only the state" in {

    val grid = State[(Int, Int), String] {
      state =>
        (state, s"Current position ---> X: ${state._1}, Y: ${state._2}")
    }

    // Mueve en 1 la posición en X y en Y.
    val run11 = State[(Int, Int), String] {
      state =>
        ((state._1 + 1, state._2 + 1), s"Current position ---> X: ${state._1}, Y: ${state._2}")
    }

    val res = for {
      firstStep <- grid
      secondStep <- run11
    }
      yield (firstStep, secondStep)

    res.runS((0, 0)).value shouldBe (1, 1)

  }

  /**
    * En este caso obtenemos una tupla de resultados y es contra lo que estamos
    * evaluando en esta prueba. ¿Se le hace familiar esto?
    */

  it should "Get only the intermediate result" in {
    val grid = State[(Int, Int), String] {
      state =>
        (state, s"Step 0: Current position ---> X: ${state._1}, Y: ${state._2}")
    }

    // Mueve en 1 la posición en X y en Y.
    val run11 = State[(Int, Int), String] {
      state =>
        val newState = (state._1 + 1, state._2 + 1)
        (newState, s"Step 1: Current position ---> X: ${newState._1}, Y: ${newState._2}")
    }

    val res = for {
      firstStep <- grid
      secondStep <- run11
    }
      yield (firstStep, secondStep)


    res.runA((0, 0)).value shouldBe ("Step 0: Current position ---> X: 0, Y: 0", "Step 1: Current position ---> X: 1, Y: 1")
  }


  it should "Demonstrate the different operators for State" in {
    /**
      * .get simplemente extrae el estado como resultado.
      * Note algo curioso: desde State se está creando un nuevo contexto monádico
      * sin aún decirle qué función de transformación tendrá o cuál será el tipo de su
      * resultado inmediato o del mismo estado. Cats inmediatamente infiere el tipo del estado
      * como el mismo tipo de retorno del cómputo en esta fase (a no ser que se lo especifiquemos
      * explícitamente como en los otros test cases).
      */
    val step1: State[(Int, Int), (Int, Int)] = State.get[(Int, Int)]
    val resStep1 = step1.run((0, 0))

    resStep1.value._1 shouldBe (0, 0)
    resStep1.value._2 shouldBe (0, 0)

    /**
      * Contrario al caso anterior, .set crea nuestro contexto monádico con el tipo
      * que le pasemos como el del estado, pero el resultado del cómputo inmediato es de tipo
      * Unit.
      * En este caso, como ya habíamos dicho que seteara el estado a la tupla de coordenadas (0, 1)
      * cualquier valor que le pasemos a la función run no hará ningún efecto.
      */
    val step2: State[(Int, Int), Unit] = State.set[(Int, Int)](0, 1)
    val resStep2 = step2.run((0, 0))

    resStep2.value._1 shouldBe (0, 1)
    resStep2.value._2 === Unit

    /**
      * .pure tiene exactamente el mismo efecto que en el caso de Writer (y de cualquier
      * otro contexto monádico que lo use): levantar un valor a su respectiva mónada.
      * Para más información respecto a esta por favor echar un ojo a la suite de pruebas de
      * Writer.
      */
    val step3: State[(Int, Int), String] = State.pure[(Int, Int), String]("Result")
    val resStep3 = step3.run((0, 0))

    resStep3.value._1 shouldBe (0, 0)
    resStep3.value._2 shouldBe "Result"

    /**
      * .inspect nos sirve para extraer el estado aplicándole una transformación.
      * En este caso nuestro estado es la tupla de coordenadas, la cual se convertirá
      * en un String con el signo ! concatenado.
      */
    val step4: State[(Int, Int), String] = State.inspect[(Int, Int), String](_ + "!")
    val resStep4 = step4.run((0, 0))

    resStep4.value._1 shouldBe (0, 0)
    resStep4.value._2 shouldBe "(0,0)!"


    /**
      * .modify, como su nombre lo indica, modifica el estado en un momento
      * dado con una función de transformación
      */
    val step5: State[(Int, Int), Unit] = State.modify[(Int, Int)](x => (x._1 + 1, x._2 + 1))
    val resStep5 = step5.run((0, 0))

    resStep5.value._1 shouldBe (1, 1)
    resStep5.value._2 === Unit
  }


}
