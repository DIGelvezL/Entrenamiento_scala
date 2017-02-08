package co.com.scalatraining.collections

import org.scalatest.FunSuite

class MapSuite extends FunSuite {

  test ("Creacion vacia") {
      val mapa1 = Map()
      val mapa2 = Map.empty
      assert(mapa1.isEmpty)
      assert(mapa2.isEmpty)
  }

  test("Un Map se debe poder operar en un for-comp"){
    val mapa = Map(1->"uno", 2->"dos")

    val res: Map[Int, String] = for{
      i <- mapa
      if i._1 == 1
    } yield(i)

    assert(res.keys.size === 1)
    assert(res.keys.head === 1)
    assert(res.get(mapa.keys.head).get === "uno")
  }

  test("Probando Map en un for-comp"){
    val mapa = Map(1->"dos", 2->"dos", 3->"tres")

    val res: Map[Int, String] = for{
      i <- mapa
      if i._2 == "dos"
    } yield(i)

    println(res)
    println(res.keys.size)
    println(res.keys.head)
    println(res.get(mapa.keys.head).get)
    assert(res.keys.size === 2)
    assert(res.keys.head === 1)
    assert(res.get(mapa.keys.head).get === "dos")
  }

  test("mapValue en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 4, "3" -> 9)) {
      map.mapValues(valor => valor * valor)
    }
  }

  test("Probando mapValue en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    println(map.mapValues(valor => valor + valor + valor))
    assertResult(Map("1" -> 3, "2" -> 6, "3" -> 9)) {
      map.mapValues(valor => valor + valor + valor)
    }
  }

  test("head en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult("1" -> 1) {
      map.head
    }
  }


  test("tail en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.tail
    }
  }

  test("split en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    val (map2, map3) = map.splitAt(2)
    assert(map2 == Map("1" -> 1, "2" -> 2) && map3 == Map("3" -> 3))
  }

  test("Probando split en un Map") {
    val map = Map("4" -> 4, "5" -> 5, "1" -> 1, "2" -> 2, "3" -> 3)
    val (map2, map3) = map.splitAt(2)
    println(map2)
    println(map3)
    assert(map2 == Map("4" -> 4, "5" -> 5) && map3 == Map("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("crear nuevo Map con un item mas") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)) {
      map + ("4" -> 4)
    }
  }

  test("Probando nuevo Map con un item mas") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6)) {
      map + ("4" -> 4) + ("5" -> 5) + ("6" -> 6)
    }
  }


  test("drop en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.drop(1)
    }
  }

  test("Probando drop en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    println(map.drop(2))
    assertResult(Map("3" -> 3)) {
      map.drop(2)
    }
  }

  test("dropRight en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2)) {
      map.dropRight(1)
    }
  }


  test("filter en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)
    assertResult(Map("2" -> 2, "4" -> 4)) {
      map.filter(dato =>
        dato._2 % 2 == 0
      )
    }
  }

  test("foreach en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(6) {
      var sum = 0
      map.foreach((x) =>
        sum += x._2
      )
      sum
    }
  }

}
