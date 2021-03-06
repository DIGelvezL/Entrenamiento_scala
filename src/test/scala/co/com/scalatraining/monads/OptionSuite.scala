package co.com.scalatraining.monads

import org.scalatest.FunSuite

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class OptionSuite extends FunSuite {

  test("Se debe poder crear un Option con valor"){
    val s = Option{
      1
    }
    assert(s == Some(1))
  }

  test("Se debe poder crear un Option para denotar que no hay valor"){
    val s = None
    assert(s == None)
  }

  test("Es inseguro acceder al valor de un Option con get"){
    val s = None
    assertThrows[NoSuchElementException]{
      val r = s.get
    }


  }

  test("Se debe poder hacer pattern match sobre un Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    var res = ""
    nombre match {
      case Some(nom) => res = nom
      case None => res = "NONAME"
    }
    assert(res == "NONAME")
  }

  test("Se debe poder saber si un Option tiene valor con isDefined") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    assert(nombre.isDefined)
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(0)
    //val res = nombre.getOrElse("NONAME")
    val res: String = nombre.getOrElse("NONAME")
    assert(res == "Andres")
  }

  test("Un Option se debe poder transformar con un map") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(1)
    val nombreCompleto = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "NONAME")
  }

  test("Un Option se debe poder transformar con flatMap en otro Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(0)

    val resultado = nombre.flatMap(s => Option(s.toUpperCase))
    resultado.map( s => assert( s == "ANDRES"))
  }

  test("Un Option se debe poder transformar con una funcion") {
    def foo(i:Option[Int]):Option[Int]={
      i.map(i => i + 1)
    }
    val lista = List(Some(1), None)
    val res: Seq[Option[Int]] = lista.map(foo)

    assert( res == List(Some(2), None))
  }

  test("Una funcion que devuelve Option si se le hace map devuelve Option(Option[])") {
    def foo(i:Int):Option[Int]={
      Option(i)
    }
    val o = Option(1)
    val res: Option[Option[Int]] = o.map(foo)
    assert( res == Some(Some(1)))
  }

  test("Una funcion que devuelve Option si se le hace flatMap devuelve Option[]") {
    def foo(i:Int):Option[Int]={
      Option(i)
    }
    val o = Option(1)
    val res: Option[Int] = o.flatMap(foo)
    assert( res == Some(1))
  }

  test("Una funcion que devuelve Future no compila si se hace flatMap") {
    def foo(i:Int):Future[Int]={Future(i)}
    val o = Option(1)

    assertDoesNotCompile("val res = o.flatMap(foo)")
  }

  test("map dentro de flatMap") {
    def sumar(i:Int, j:Int)=i+j
    val o1 = Option(1)
    val o2 = Option(2)

    val r1: Option[Option[Int]] = o1.map(v1 => o2.map(v2 => sumar(v1, v2)))
    val r2: Option[Int] = o1.flatMap(v1 => o2.map(v2 => sumar(v1, v2)))
    val r3: Option[Int] = o1.flatMap(v1 => o2.flatMap(v2 => Option(sumar(v1, v2))))

    assert(r1 != r2)
    assert(r2 == r3)
  }

  test("Un Option se debe poder filtrar con una hof con filter") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista(0)
    val option1 = lista(2)
    val res0 = option0.filter(_>10)
    val res1 = option1.filter(_>10)

    assert(res0 == None)
    assert(res1 == Some(40))
  }

  test("for comprehensions en Option") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)
    val resultado = for {
      x <- s1
      y <- s2
    } yield x+y
    assert(resultado == Some(45))
  }

  test("for comprehesions None en Option") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad = None
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad
      sex <- consultarSexo
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda, $sex")

    assert(resultado == None)
  }

  test("for comprehesions None en Option 2") {

    def consultarNombre(dni:String): Option[String] = Some("Felix")
    def consultarApellido(dni:String): Option[String] = Some("Vergara")
    def consultarEdad(dni:String): Option[Int] = Some(27)
    def consultarSexo(dni:String): Option[String] = Some("M")

    val dni = "8027133"
    val resultado = for {
      nom <- consultarNombre(dni)
      ape <- consultarApellido(dni)
      eda <- consultarEdad(dni)
      sex <- consultarSexo(dni)
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda, $sex")

    assert(resultado != None)
  }

  test("for comprehesions en Option(Option[T])") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad: Option[Option[Int]] = Option(Option(27))
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad.flatMap(x => x)
      sex <- consultarSexo
    } yield (s"$nom $ape, $eda, $sex")

    assert(resultado != None)
  }

  test("Probando None en for") {
    val a = Some(1)
    val b = None

    def c = {println("prueba")
      Option(3)
      }

    val resultado = for {
      x <- a
      y <- b
      z <- c
    } yield x + y + z
    assert(resultado == None)
  }

}

