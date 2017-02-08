package co.com.scalatraining.collections

import org.scalatest.FunSuite

/**
  * Created by daniel on 8/02/17.
  */
class SeqRangeSuite extends FunSuite {

  test ("Creacion de range") {
    val x = (1 to 10).toList
    val y = (1 to 10).toArray
    val z = (1 to 10).toSet
    val a = Vector.range(1, 10)

    println(x)
    println(y)
    println(z)
    println(a)

  }

}
