package co.com.scalatraining.PatternMatching

import org.scalatest.FunSuite

/**
  * Created by daniel on 9/02/17.
  */
class PatternMatchingSuite extends FunSuite {

  test ("Pattern MAtching 1") {
    val a: Int = 2
    val b:String = a match{
      case 1 => "uno"
      case 2 => "dos"
      case 3 => "tres"
      case x => "no se que es " + x
    }

    assert(b != "uno")
  }

  test ("Pattern MAtching 2 por tipos") {
    val l = List(5, 10, 15, 20, 25, 30)
    val l2 = l match{
      case x:List[_] => "La lista tiene elementos"
      case Nil => "La lista no tiene elementos"
      case _ => "no se que es " + _
    }
    assert(l2 == "La lista tiene elementos")
  }

  test ("Pattern MAtching 3") {
    val l = List(5, 10, 15, 20, 25, 30)
    val l2 = l match{
      case x::xs => "La lista tiene elementos"
      case x::y::Nil => "La lista no tiene elementos"
      case x::y::xs => "no se que es " + _
    }
    assert(l2 == "La lista tiene elementos")
  }

  test ("Pattern MAtching 4 evaluando un case class") {
    val l = List(5, 10, 15, 20, 25, 30)
    case class Persona(nobre: String, apellido: String, edad: Int)
    val p = Persona("Daniel", "Gelvez", 27)
    val pe = p match{
      case Persona(_, "Gelvez", 26) => "Probablemente es la persona correcta"
      case Persona("Daniel", "Gelvez", 27) => "Es la persona correcta"
      case Persona(_, _, _) => "Probablemente no es la persona correcta"
      case _ => "no se que es " + _
    }
    assert(pe != "Probablemente es la persona correcta")
  }

  test ("Pattern MAtching 5 usando funcion") {
    val a: Int = 5
    def f(a: Int)={
      a % 2 == 0
    }
    val b:String = f(a) match{
      case true => "Par"
      case false => "Impar"
    }
    assert(b == "Impar")
  }

  test ("Pattern MAtching 6 usando head") {
    val l = List(5, 10, 15, 20, 25, 30)
    def f(a: Int)={
      a % 2 == 0
    }

    var b = f(l.head) match{
      case true => "Par"
      case false => "Impar"
    }

    assert(b == "Impar")
  }

  test ("Pattern MAtching 7 usando map") {
    val l = List(5, 10, 15, 20, 25, 30)
    def f(a: Int)={
      a % 2 == 0
    }

    val l2 = l.map(x => f(x) match{
      case true => x + "-Par"
      case false => x + "-Impar"
    })
    assert(l != l2)
  }

  test ("Pattern MAtching 8 usando tuplas") {

   val l = List(5, 10, 15, 20, 25, 30)

    def serializar(i:Int) = {
      i%2==0 match {
        case true => "Par"
        case false => "Impar"
      }
    }

    val tupla = l.map(n => (n, serializar(n)))

    val tuplas = l.map(n => {
        val t = (n, serializar(n))
        //println(t)
        t
      })

    assert(l != tupla)
  }

  test ("Pattern MAtching 8 usando trait") {

    trait Calculator {

      def suma(a: Double, b:Double):Double = {
        a + b
      }

      def resta(a: Double, b:Double):Double = {
        a - b
      }

      def multiplicacion(a: Double, b:Double):Double = {
        a * b
      }

      def division(a: Double, b:Double):Double = {
        a / b
      }
    }

    trait Ser {

      def serializar(i: Int) = {
        i % 2 == 0 match {
          case true => "Par"
          case false => "Impar"
        }
      }
    }

    case class Profesor(nombre:String)
    case class Curso(nombre:String, profesor:Profesor)

    val c1 = new Curso("Scala", Profesor("DG")) with Calculator
    val c2 = new Curso("Scala", Profesor("IL")) with Ser

    def foo(c:Curso) = {
      c match {
        case x:Curso with Calculator if x.profesor.nombre == "DG" => //x.suma()
        case x:Curso => x.profesor.nombre
      }
    }

  }

  test ("Pattern ") {

    val myMap = Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
    intercept[NoSuchElementException] {
      myMap("TX")
    }
    myMap.getOrElse("TX", "missing data")

    println(myMap.getOrElse("TX", "missing data"))
  }

}
