import org.scalatest.{FlatSpec, Matchers}
import cats.data.Writer
import cats.implicits._

class WriterTestSuite extends FlatSpec with Matchers{

  "WriterTestSuite" should "pass a basic smoke test" in {
    assert(true)
  }

  it should "Demonstrate the creation of a basic Writer" in {
    /**
      * Un Writer es un contexto monádico ("Mónada") la cual almacena un valor junto con un log
      * el cual puede representar información acerca del valor computado (cómo se hizo? qué es? ó
      * cualquier cosa que sea necesaria saber respecto a ese resultado).
      * La aplicación más poderosa (pero no la única) de writer es para manejar logs, sobre todo
      * en ambientes distribuídos en donde estos pueden aparecer en desorden, haciendo muy difícil
      * el diagnóstico u obtención de información del cómputo al que le queremos hacer trazabilidad.
      *
      * Por último, hay que tener en cuenta que los logs son un side effect más y que Writer nos ayuda
      * a manejar estos de una manera funcional.
      */
    val writer = Writer("Normal font size", 12)

    writer.run shouldBe ("Normal font size", 12)
  }

  it should "Use tell syntax for appending a log message" in {

    /**
      * tell es una función de Writer que nos permite hacer un "append" a un writer que ya esté creado
      * para adicionar más información al log.
      */
    val writer1 = Writer("Normal font size", 12)
    val writer2 = writer1.tell("Going to increase font size")

    writer2 shouldBe Writer("Normal font sizeGoing to increase font size", 12)
  }

  /**
    * Vamos a ver en el siguiente par de tests las funciones de transformación
    * que Cats ofrece para trabajar con Writer.
    * En esta primera prueba veremos un Map "normalito" el cuál transforma el valor
    * del Writer con cualquier función que le pasemos.
    */
  it should "Transform the value" in {
    val originalWriter = Writer("", 12)

    val writerValueTransformed = originalWriter.map{
      x =>
        x * 2
    }

    val writerValueAndLogTransformed = writerValueTransformed.tell("Doubled the original size of the font")

    writerValueAndLogTransformed shouldBe Writer("Doubled the original size of the font", 24)
  }

  /**
    * En este test estamos usando la función "mapWritten" la cual permite transformar el log haciendo
    * las manipulaciones que queramos, nuevamente, pasándole cualquier función.
    */
  it should "Transform the log" in {
    val originalWriter = Writer("I'm using a nice normal-sized font! :)", 12)
    val modifiedWriterLog = originalWriter.mapWritten(_ + " . But it's Comic Sans!".toUpperCase)

    modifiedWriterLog shouldBe Writer("I'm using a nice normal-sized font! :) . BUT IT'S COMIC SANS!", 12)
  }

  /**
    * En este test estamos usando biMap, una función que recibe a su vez dos funciones, una
    * para cada valor del writer (log y valor como tal). Estas transformarán los valores de este
    * contexto monádico.
    */
  it should "Transform both values with biMap" in {
    val originalWriter = Writer("I'm using Comic Sans MS", 12)
    val transformedWriter = originalWriter.bimap(
      log => log + ". AWFUL! IT'S BETTER TO CHANGE THE SIZE",
      result => result - 11 // Vamos a eliminar lo feo de Comic Sans dejándola en tamaño 1 :)
    )

    transformedWriter shouldBe Writer("I'm using Comic Sans MS. AWFUL! IT'S BETTER TO CHANGE THE SIZE", 1)
  }

  /**
    * mapBoth sirve el mismo propósito de biMap pero con una estructura ligeramente diferente:
    * recibe una función de dos parámetros los cuales representan el log y el valor y luego de aplicar
    * la transformación desea se retorna una tupla de los valores evaluados.
    */
  it should "Transform both values with mapBoth" in {
    val originalWriter = Writer("I'm using Comic Sans MS", 12)
    val transformedWriter = originalWriter.mapBoth{
      (log, result) =>
        val newLog = log + ". AWFUL! IT'S BETTER TO CHANGE THE SIZE"
        val newResult = result - 11
        (newLog, newResult)
    }

    transformedWriter shouldBe Writer("I'm using Comic Sans MS. AWFUL! IT'S BETTER TO CHANGE THE SIZE", 1)
  }

  /**
    * Similar a Reader, Writer posee la función run la cual evalúa el contexto de la mónada y produce una
    * tupla con los valores que esta contiene.
    */
  it should "Run the computation" in {
    val writer = Writer("I'm using Comic Sans MS", 12)

    /**
      * Note que al usar .run usted ya se "libera" del contexto monádico (wraaper de Writer)
      * y queda con la Tupla de valores, con la cual ya usted podrá extraer (cosa que igual puede hacer
      * con el contexto) y operar sobre sus valores. Recuerde que la diferencia aquí es que ya las propiedades
      * monádicas (sobre todo la composición) se pierde.
      */
    writer.run shouldBe ("I'm using Comic Sans MS", 12)
  }


  /**
    * Si en algún momento finalizamos nuestro use case con nuestro contexto monádico y queremos
    * reutilizarlo podemos limpiar el log acumulado con la función reset.
    */
  it should "Clean the log" in {
    val writer = Writer("I'm thinking about stop using Comic Sans MS", 12)

    writer.reset shouldBe Writer("", 12)
  }


  /**
    * Similar a Either, yo puedo hacer un swap de los valores. Por convención
    * el log está en el lado izquierdo y el valor en el derecho. Con Swap puedo lograr cambiar
    * este orden.
    */
  it should "Swap the values" in {
    val writer = Writer("Normal font size", 12)

    writer.swap shouldBe Writer(12, "Normal font size")
  }

}
