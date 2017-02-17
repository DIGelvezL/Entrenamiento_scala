package co.com.scalatraining.syntax

import org.scalatest.FunSuite

/**
  * Created by daniel on 17/02/17.
  */
class EitherSuite extends FunSuite {

  test("Construyendo un Either"){
    val r: Either[Nothing, Int] = Right(1)
    val l = Left("Hola soy un Left")

    assert(r.isRight)
    assert(l.isLeft)
  }

  test("Construyendo un Either 2"){
    def foo(i:Int) ={
      if(i%2==0) Right(i)
      else Left("Soy un impar triste")
    }

    val res: Either[String, Int] = foo(2)

    assert(res == Right(2))

  }

  test("List es un Either "){
    val listIn = List("Hola", "5", "Avión", "27")

    def foo(in:String) = try {
      Right(in.toInt)
    } catch {
      case e: Exception =>
        Left(in)
    }

    val result = listIn.map(x => foo(x))

    val res = result.map(x => x match {
      case Right(x) => "El Int: " + x + ", lo voy a incrementar. " + x + " + 1 = " + (x+1)
      case Left(x) => "El String: " + x
    })

    assert(res != result)

  }

  test("Left.map y Right.map en un Either "){
    val l: Either[String, Int] = Left("Futbol")
    val r: Either[String, Int] = Right(27)

    val resl1 = l.left.map(x => x.size)
    val resl2 = r.left.map(x => x.size)
    val resr1 = l.right.map(_.toDouble)
    val resr2 = r.right.map(_.toDouble)

    assert(resl1 != resl2)
    assert(resr1 != resr2)

  }

  test("map en un Either "){
    val in = 10

    def foo(i:Int) ={
      if(i%2==0) Right(i)
      else Left(i)
    }

    val either = foo(in)
    val res = either.right.map(x => x * 5)

    assert(res != Left(50))

  }

  test("joinLeft en un Either "){
    val in = 11

    def foo(i:Int) ={
      if(i%2==0) Right(Right(i))
      else Left(Left(i))
    }

    val either = foo(in)

    assert(either.joinLeft.left.get == in)

  }

  test("getOrElse en either"){
    val right: Either[String, Int] = Right(27)
    val left: Either[Int, String] = Left(27)

    val r = right.left.getOrElse(false)
    val l = left.right.getOrElse(true)

    assert(r == false)
    assert(true == l)

  }

}
