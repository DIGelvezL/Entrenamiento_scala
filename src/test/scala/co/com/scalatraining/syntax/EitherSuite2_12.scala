package co.com.scalatraining.syntax

import org.scalatest.FunSuite
import scala.util.{Failure, Success, Try}

/**
  * Created by daniel on 17/02/17.
  */
class EitherSuite2_12 extends FunSuite {

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

    test("List es un Either donde los left son false y los right se multiplican por 2"){
      val l = List(5, 4, 3, 2, 1)

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val l2 = l.map(x => foo(x))

      val l3 = l2.map(x => x match {
        case Right(x) => x * 2
        case Left(x) => false
      })

      assert(l3.head == false)

    }

    test("List es un Either envalua try donde los left son failure y los right se multiplican por 2"){
      val l = List(5, 4, 3, 2, 1)

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val l2 = l.map(x => foo(x))

      val l3 = l2.map(x => Try{x match {
        case Right(x) => x * 2
        case Left(x) => x/0
      }})

      val res = l3.filter(x => x.isSuccess)

      assert(res.head == Success(8))

    }

    test("map en un Either "){
      val in = 10

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val either = foo(in)
      val res = either.map(x => x * 5)

      assert(res != Left(50))

    }

    test("fold en un Either "){
      val in = 10

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val either = foo(in)
      val res = either.fold(l => l-5, r => r+5)

      assert(res == 15)

    }

    test("Un either se puede pasar a option"){
      val in = 10

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val res = foo(in).toOption

      assert(res == Some(10))

    }

    test("Un either se puede pasar a Seq"){
      val in = 10

      def foo(i:Int) ={
        if(i%2==0) Right(i)
        else Left(i)
      }

      val res = foo(in).toSeq

      assert(res == List(10))

    }

    test("swap en either"){
      val right = Right(2)
      val left = Left(3)

      val res = for{
        r1 <- right
        r2 <- left.swap
      }yield r1 * r2

      val l = Left(2*3)
      val r = l.swap

      assert(res == r)

    }

    test("merge en either"){
      val right: Either[String, Int] = Right(2)
      val left: Either[Int, String] = Left(2)

      val r = right.merge
      val l = left.merge

      assert(right != r)
      assert(left != l)
      assert(r == l)

    }

    test("Right de list y Lfet de list"){
      val listIn = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

      val right: Either[List[Int], List[Int]] = Right(listIn.filter(_%2==0))
      val left: Either[List[Int], List[Int]] = Left(listIn.filter(_%2!=0))

      assert(right != left)

    }

  test("filterOrElse en either"){

    def foo(i:Int) ={
      if(i%2==0) Right(i)
      else Left(i)
    }

    val res = foo(28).filterOrElse(_ > 20, -5)
    val res2 = foo(26).filterOrElse(_ > 28, 17)
    val res3 = foo(17).filterOrElse(_ > 20, -5)

    assert(res != res2)
    assert(res3 == res2)

  }

}
