package co.com.scalatraining.monads

import java.io.Serializable
import java.util.Random
import java.util.concurrent.Executors

import org.scalatest.FunSuite

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureSuite extends FunSuite {

  test("Un futuro se puede crear") {

    val hiloPpal = Thread.currentThread().getName

    var hiloFuture = ""
    val saludo = Future {
      hiloFuture = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }

    val resultado: String = Await.result(saludo, 10 seconds)
    val resultado2 = Await.result(saludo, Duration.Inf)

    assert(resultado == "Hola")
    assert(hiloPpal != hiloFuture)
  }

  test("map en Future trabaja con el mismo hilo") {
    var hilo1 = ""
    val saludo = Future {
      hilo1 = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }

    var hilo2 = ""
    val saludoCompleto: Future[String] = saludo.map(mensaje => {
      hilo2 = Thread.currentThread().getName
      mensaje + " muchachos"
    })

    val resultado = Await.result(saludoCompleto, 10 seconds)
    assert(resultado == "Hola muchachos")
    assert(hilo1 == hilo2)
  }

  test("flatMap en Future") {
    var hilo1 = ""
    val saludo = Future {
      hilo1 = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }

    var hilo2 = ""
    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    val saludoCompleto: Future[String] = saludo.flatMap{mensaje => Future{
        hilo2 = Thread.currentThread().getName
        mensaje + " muchachos"
      }(ecParaPrimerHilo)
    }

    val resultado = Await.result(saludoCompleto, 10 seconds)

    assert(resultado == "Hola muchachos")
    assert(hilo1 != hilo2)
  }

  test("2 Future en un flatMap, el primer future trabaja en un hilo diferente y el segundo con el mismo hilo del principal") {
    var hilo1 = ""
    val saludo = Future {
      hilo1 = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }

    var hilo2 = ""
    var hilo3 = ""
    val saludoCompleto = saludo.flatMap{ mensaje => Future{
      hilo2 = Thread.currentThread().getName
      "segundo future"}
      Future{
        hilo3 = Thread.currentThread().getName
        mensaje + " muchachos"
      }}

    val resultado = Await.result(saludoCompleto, 10 seconds)
    assert(resultado == "Hola muchachos")
    assert(hilo1 != hilo2)
    assert(hilo1 == hilo3)
  }

  test("Un Future dentro de otro Future en flatMap ") {
    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
    var hilo1 = ""
    val saludo = Future {
      hilo1 = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }(ecParaPrimerHilo)

    var hilo2 = ""
    var hilo3 = ""
    val saludoCompleto: Future[Future[String]] = saludo.flatMap { mensaje =>
      implicit val ecParaFlatMap = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
      Future {
        hilo2 = Thread.currentThread().getName
        Future {
          hilo3 = Thread.currentThread().getName
          mensaje + " muchachos"
        }(ecParaFlatMap)

      }(ecParaFlatMap)
    }

    val resultado = Await.result(saludoCompleto, 10 seconds)
    val resultadoFinal = Await.result(resultado, 10 seconds)

    assert(resultadoFinal == "Hola muchachos")
    assert(hilo1 != hilo2)
    assert(hilo2 == hilo3)
  }

  test("Se debe poder encadenar Future con for-comp") {
    val f1 = Future {
      Thread.sleep(200)
      1
    }

    val f2 = Future {
      Thread.sleep(200)
      2
    }

    val f3: Future[Int] = for {
      res1 <- f1
      res2 <- f2
    } yield res1 + res2

    val res = Await.result(f3, 10 seconds)

    assert(res == 3)

  }

  test("Se debe poder manejar el error de un Future de forma imperativa") {
    val divisionCero = Future {
      Thread.sleep(100)
      10 / 0
    }
    var error = false

    val r = divisionCero.onFailure {
      case e: Exception => error = true
    }

    Thread.sleep(1000)

    assert(error == true)
  }

  test("Se debe poder manejar el exito de un Future de forma imperativa") {

    val division = Future {
      5
    }

    var r = 0

    val f = division.onComplete {
      case Success(res) => r = res
      case Failure(e) => r = 1
    }

    Thread.sleep(150)

    val res = Await.result(division, 10 seconds)

    assert(r == 5)
  }

  test("Se debe poder manejar el error de un Future de forma funcional sincronicamente") {

    var threadName1 = ""
    var threadName2 = ""

    val divisionPorCero = Future {
      threadName1 = Thread.currentThread().getName
      Thread.sleep(100)
      10 / 0
    }.recover {
      case e: ArithmeticException => {
        threadName2 = Thread.currentThread().getName
        "No es posible dividir por cero"
      }
    }

    val res = Await.result(divisionPorCero, 10 seconds)

    assert(threadName1 == threadName2)
    assert(res == "No es posible dividir por cero")

  }

  test("Se debe poder manejar el error de un Future de forma funcional asincronamente") {

    var threadName1 = ""
    var threadName2 = ""

    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    val f1 = Future {
      threadName1 = Thread.currentThread().getName
      2/0
    }(ecParaPrimerHilo)
    .recoverWith {
      case e: ArithmeticException => {

        implicit val ecParaRecuperacion = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

        Future{
          threadName2 = Thread.currentThread().getName
          1
        }(ecParaRecuperacion)
      }
    }

    val res = Await.result(f1, 10 seconds)

    assert(threadName1 != threadName2)
  }

  test("Los future **iniciados** fuera de un for-comp deben iniciar al mismo tiempo") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val additionalTime = 50D

    val estimatedElapsed = (Math.max(Math.max(timeForf1, timeForf2), timeForf3) + additionalTime)/1000

    val f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    val f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    val f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed <= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future **definidos** fuera de un for-comp deben iniciar secuencialmente") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3)/1000

    def f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    def f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    def f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future declarados dentro de un for-comp deben iniciar secuencialmente") {

    val t1 = System.nanoTime()

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3)/1000

    val resultado = for {
      a <- Future {
        Thread.sleep(timeForf1)
        1
      }
      b <- Future {
        Thread.sleep(timeForf2)
        2
      }
      c <- Future {
        Thread.sleep(timeForf3)
        3
      }
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)
  }

  test("Lista de Future con recover") {
    val lista = List(5, 4, 3)
    val lista2 = List(2, 1, 0)

    def foo(l1:List[Int], l2:List[Int]) ={
      List(
        Future{
          l1.map(x => 5/x)
        }.recover {
          case e: ArithmeticException => {
            "No es posible dividir por cero"
          }},
        Future{
          l2.map(x => 5/x)
        }.recover {
          case e: ArithmeticException => {
            "No es posible dividir por cero"
          }}
      )
    }

    val resultado = foo(lista, lista2)
    val res1 = Await.result(resultado.head, 10 seconds)
    val res2 = Await.result(resultado.last, 10 seconds)

    assert(res1 != res2)
  }

  test("futuro en un for"){
    val lista2 = List(2, 1, 0)

    def f1(i:Int) ={
      Future{
        Thread.sleep(2000)
        10/i
      }.recover {
        case e: ArithmeticException => {
          0
        }}
    }

    def f2(i:Int) ={
      Future{
        Thread.sleep(2000)
        i + 5
      }
    }

    val resultado = for {
      a <- lista2.map(x => f2(x)).map(x => Await.result(x, 10 seconds))
      b <- lista2.map(x => f1(x)).map(x => Await.result(x, 10 seconds))
    } yield List(a, b)

    assert(resultado.flatten != lista2)

  }

  test("Probando el inicio de hilos al mismo tiempo") {

    val f1 = Future{
      Thread.sleep(2000)
      10
    }

    val f2 = Future{
      Thread.sleep(2000)
      5
    }

    val resultado = for {
      a <- f1
      b <- f2
    } yield a * b

    val res = Await.result(resultado, 10 seconds)

    assert(res == 50)

  }

  test("loop infinito") {
    def f(i:Int): Int ={
      i+1
    }

    while (true){
      val res = Future{
        println(Thread.currentThread().getName)
        f(5)
      }
    }
  }

  test("Flatten en futuro"){
    val lista2 = List(2, 1, 0)

    def f1(i:Int) ={
      Future{
        Thread.sleep(100)
        10/i
      }.recover {
        case e: ArithmeticException => {
          0
        }}
    }

    val resultado = lista2.map(x => Future{f1(x)})//.flatMap(x => Future{Await.result(x, 10 seconds)})
    val listRes = resultado.map(l => Await.result(l.flatten, 10 seconds))

    assert(listRes == List(5, 10, 0))

  }

  test("zipWith en futuro"){
    def f1(i:Int) ={
      Future{
        Thread.sleep(100)
        10/i
      }.recover {
        case e: ArithmeticException => {
          "No se puede dividir por: 0"
        }}
    }

    val fut1 = f1(2)
    val fut2 = f1(0)

    val zipWith = fut1.zipWith(fut2)((f1, f2) => s"La función uno es $f1 & la función dos $f2")
    val res = Await.result(zipWith, 10 seconds)

    assert(res != List(5, 10, 0))

  }

  test("Transform en futuro"){
    def f1(i:Int) ={
      Future{
        Thread.sleep(100)
        10/i
      }.recover {
        case e: ArithmeticException => {
          "No se puede dividir por: 0"
        }}
    }

    val fut1 = f1(2)
    val fut2 = f1(0)

    val t1 = Await.result(fut1.transform(Try(_)), 10 seconds)
    val t2 = Await.result(fut2.transform(Try(_)), 10 seconds)

    assert(t1.isSuccess)
    assert(t1 != t2)

  }

  test("Transform2 en futuro"){

    val fut2 = Future{5}

    val res = fut2.transform(x => Try{x.map(y => y*5)})
    val res2 = fut2.transform(x => x.map(y => y*5))

    val t1 = Await.result(res, 10 seconds)
    val t2 = Await.result(res2, 10 seconds)

    assert(t1.isSuccess)

  }

  test("transformWith en futuro"){
    def f1(i:Int) ={
      Future{
        Thread.sleep(100)
        10/i
      }.recover {
        case e: ArithmeticException => {
          "No se puede dividir por: 0"
        }}
    }

    val fut1 = f1(2)
    val fut2 = f1(0)

    val t1 = Await.result(fut1.transformWith(x => Future{x}), 10 seconds)
    val t2 = Await.result(fut2.transformWith(x => Future{x}), 10 seconds)

    assert(t1.isSuccess)
    assert(t1 != t2)

  }

  test("foreach en futuro"){
    def f1(i:Int) ={
         i/2

    }

    val fut = Future{10}

    val r1: Unit = fut.foreach(x => f1(x))

    assert(Await.result(fut, 10 seconds) != fut.foreach(x => f1(x)))
  }

  test("unit en futuro"){
    def foo(s: String) = s match {
      case null | "" => Future.unit
      case "Hola" => Future{s"$s => es un complemento!"}
    }

    val f1 = Future{""}
    val f2 = Future{"Hola"}
    val r1 = f1 flatMap foo
    val r2 = f2 flatMap foo

    assert(r1 != r2)

  }

}