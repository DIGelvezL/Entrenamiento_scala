package co.com.scalatraining.collections

import org.scalatest.FunSuite

/**
  * Created by daniel on 8/02/17.
  */
class VectorSuite extends FunSuite {

  test ("Creacion vacia") {
    val vector = Vector()
    val vector2 = Vector.empty
    assert(vector.isEmpty)
    assert(vector2.isEmpty)
  }

  test ("Agregar un nuevo valor") {
    val vector = Vector(5, 10)
    val vector2 = vector :+ 20 :+ 25
    assert(!vector.isEmpty)
    assert(!vector2.isEmpty)
  }

  test ("Agregar un valor al inicio de otro vector") {
    val vector = Vector(5, 10)
    val vector2 = 100 +: vector
    assert(!vector.isEmpty)
    assert(!vector2.isEmpty)
  }

  test("head en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(5) {
      vector.head
    }
  }

  test("tail en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(Vector(10, 15, 20, 25)) {
      vector.tail
    }
  }

  test("split en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    val (v1, v2) = vector.splitAt(3)
    assert(v1 == Vector(5, 10, 15) && v2 == Vector(20, 25))
  }

  test("drop en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(Vector(20, 25)) {
      vector.drop(3)
    }
  }

  test("dropRight en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(Vector(5, 10, 15, 20)) {
      vector.dropRight(1)
    }
  }

  test("filter en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(Vector(5, 15, 25)) {
      vector.filterNot(x => x % 2 == 0)
    }
  }

  test("foreach en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult(375000) {
      var mult = 1
      vector.foreach(x => mult *= x)
      mult
    }
  }

  test("Un Vector se debe poder operar en un for-comp"){
    val vector = Vector(5, 10, 15, 20, 25)

    val res = for{i <- vector if i == 15} yield(i)
    assert(res === Vector(15))
  }

  test("Una Vector se debe poder transformar") {

    def f(i:Int):Int = i + i

    val vector: Seq[Int] = Vector(5, 10, 15, 20, 25)
    val vector2 = vector.map(x => x + x)
    val vector3 = vector.map(x => f(x))

    assert(vector2.head == 10)
    assert(vector != vector2)
    assert(vector2 == vector3)
  }

  test("fold en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)

    assertResult(77) {
      vector.fold(2) {
        (acumulado, item) => acumulado + item
      }
    }
  }

  test("Probar fold en tuplas en un Vector"){
    val vector = Vector((5, "cinco"), (10, "ten"), (15, "quince"), (20, "veinte"), (25, "hello"))

    def f(a:(Int, String), b:(Int, String)):(Int, String) = {
      ((a._1 + b._1), (a._2 + b._2 + "-"))
    }

    val v1 = vector.fold((0,"")) {(acumulado: (Int, String), item: (Int, String)) => f(acumulado, item)}
    val v2 = (75, "cinco-ten-quince-veinte-hello-")
    assert(v1 == v2)

  }

  test("mkString en un Vector") {
    val vector = Vector(5, 10, 15, 20, 25)
    assertResult("5&10&15&20&25") {
      vector.mkString("&")
    }
  }

}
