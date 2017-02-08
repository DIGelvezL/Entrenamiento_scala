package co.com.scalatraining.collections

import org.scalatest.FunSuite

import scala.collection.immutable.HashMap

/**
  * Created by daniel on 8/02/17.
  */
class HashMapSuite extends FunSuite {

  test ("Creacion de hashmap") {
    val hm = HashMap(0 -> 1, 2 -> 3)

    assertResult(Some(3)) {
      hm.get(2)
    }

  }

  test ("forAll en un hashmap") {
    val hm = HashMap(1 -> 10, 2 -> 20)

    val areAllValuesTenTimesTheKey = hm.forall(p => p._1 * 10 == p._2)
    assert(areAllValuesTenTimesTheKey)

  }

}
