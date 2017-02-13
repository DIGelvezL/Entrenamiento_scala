package co.com.scalatraining.monads

import org.scalatest.FunSuite
import scalaz._
import Scalaz._

/**
  * Created by daniel on 13/02/17.
  */
class OptionCheatSheet extends FunSuite {

  test("Cheat Sheet de flatMap Option") {
    def foo(i:Option[Int]):Option[Int]={
      i.map(i => i + 1)
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => None
      case Some(x) => foo(Option(x))
    }

    assert(resul == Some(16))

    val res = lista(0).flatMap(x => foo(Option(x)))

    assert(res == Some(16))
  }

  test("Cheat Sheet de flatten Option") {
    val lista = List(Some(Some(15)), None)

    val resul = lista(0) match {
      case None => None
      case Some(x) => x
    }

    assert(resul == Some(15))

    val res = lista(0).flatten

    assert(res == Some(15))
  }

  test("Cheat Sheet de map Option") {
    def foo(i:Int):Int={
      i + 1
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => None
      case Some(x) => Some(foo(x))
    }

    assert(resul == Some(16))

    val res = lista(0).map(foo)

    assert(res == Some(16))
  }

  test("Cheat Sheet de foreach Option") {
    def foo(i:Int):Int={
      i + 1
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => {}
      case Some(x) => foo(x) + 3
    }

    assert(resul == 19)

    var res = 0
    lista(0).foreach(x => res = (foo(x) + 3))

    assert(res == 19)
  }

  test("Cheat Sheet de isDefined Option") {
    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => false
      case Some(_) => true
    }

    assert(resul == true)

    var res = lista(0).isDefined

    assert(res == true)
  }

  test("Cheat Sheet de isEmpty Option") {
    def foo(i:Int):Int={
      i + 1
    }

    val lista = List(Some(15), None)

    val resul = lista(1) match {
      case None => true
      case Some(_) => false
    }

    assert(resul == true)

    var res = lista(1).isEmpty

    assert(res == true)
  }

  test("Cheat Sheet de forall Option") {
    def foo(i:Int):Boolean={
      i % 2 == 0
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => true
      case Some(x) => foo(x)
    }

    assert(resul == false)

    val res = lista(0).forall(foo)

    assert(res == false)
  }

  test("Cheat Sheet de exists Option") {
    def foo(i:Int):Boolean={
      i % 2 == 0
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => false
      case Some(x) => foo(x)
    }

    assert(resul == false)

    val res = lista(0).exists(foo)

    assert(res == false)
  }

  test("Cheat Sheet de orElse Option") {
    def foo():Option[Int]={
      Option(5)
    }

    val lista = List(Some(15), None)

    val resul = lista(1) match {
      case None => foo
      case Some(x) => Some(x)
    }

    assert(resul == Some(5))

    val res = lista(1).orElse(foo)

    assert(res == Some(5))
  }

  test("Cheat Sheet de getOrElse Option") {
    def foo():Option[Int]={
      Option(5)
    }

    val lista = List(Some(15), None)

    val resul = lista(0) match {
      case None => foo
      case Some(x) => x
    }

    assert(resul == 15)

    val res = lista(0).getOrElse(foo)

    assert(res == 15)
  }

  test("Cheat Sheet de toList Option") {
    def foo():Option[Int]={
      Option(5)
    }

    val o1 = Some(15)
    val o2 = None

    val resul = o1 match {
      //case None => Nil
      case Some(x) => x :: Nil
    }

    assert(resul == List(15))

    val res = o1.toList
    val resList = res :: o2.toList :: Nil

    assert(resList.flatten == List(15))
  }

  test("Cheat Sheet de coflatMap Option") {
    def foo(i:Option[Int]):Boolean={
      i.isEmpty
    }

    val o1 = Option(15)

    val resul = o1 match {
      case None => None
      case Some(_) => Some(foo(o1))
    }

    assert(resul == Some(false))

    val res = o1.coflatMap(foo)

    assert(res == Some(false))
  }

  test("Cheat Sheet de duplicate (cojoin) Option") {
    val o1 = Option(15)

    val resul = o1 match {
      case None => None
      case Some(_) => Some(o1)
    }

    assert(resul == Some(Some(15)))

    val res = o1.cojoin

    assert(res == Some(Some(15)))
  }

}
