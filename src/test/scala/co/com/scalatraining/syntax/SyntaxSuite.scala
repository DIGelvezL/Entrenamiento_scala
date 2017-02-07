package co.com.scalatraining.syntax

import org.scalatest.FunSuite
import sun.reflect.generics.reflectiveObjects.NotImplementedException

class SyntaxSuite extends FunSuite{

  test("Un var debe permitir realizar asignaciones"){
    var x = 0
    assert(x == 0)
    x = 2
    assert(x == 2)
  }

  test("Un val no debe permitir realizar asignaciones"){
    val x = 0
    assert(x == 0)
    assertDoesNotCompile("x = 1")
  }

  test("Los tipos en Scala son iferidos por el compilador"){
    // Fijate como no hay que decir de qué tipo es x
    val x = 0
    assert(x == 0)

    // Aunque tambien lo puedes hacer explicito si asi lo quieres
    val y = "0"
    assert(y == "0")

    // Si eres incredulo fijate como el tipo es fuerte y no debil
    var strong = "0"

    assertDoesNotCompile ("strong = 1")
  }

  test("Scala no debe permitir iniciar en null"){
    var x = null
    assertDoesNotCompile("x = 1")
  }

  test("Scala no debe permitir declarar sin asignar"){
    assertDoesNotCompile("var x")
  }

  test("Un object puede tener funciones miembro"){

    object obj {

      var x = 1
      val y = 0

      def f1(a: Int, b:Int):Int = {
        x = x + 1
        a + x
      }

      def f2(a: Int) = {
        a + 2
      }

      def esPositivo(a: Int):Boolean = {
        if(a >= 0){
          true
        }else{
          false
        }
      }

      def f4(a: Int):Boolean = {
        a % 2 == 0
      }
    }

    //fijate como no hay que hacer new de obj
    val res = obj.f2(1)
    obj.x = 55
    assert(obj.x == 55)
    assert(obj.esPositivo(2))
    assert(!obj.esPositivo(-2))
    assert(obj.f4(2))
    assert(res == 3)
  }

  test("Un class se puede comoportar como un class tradicional"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){
      def f1 = a + 1
      def f2 = a + 2

    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)
    val res = mc.f1
    assert(res == 2)
  }

  test("A un class se le puede  mutar su estado"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){

      var r = 0

      def f1 = {
        r = r + 2
        a + 1
      }

      def f2 = a + 2

      def getA = a
    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)
    assert(mc.r == 0)
    val res1 = mc.f1
    assert(mc.r == 2)
    val res2 = mc.f1
    assert(mc.r == 4)
  }

  test("Probando una clase sin atributos"){

    class MyClassSinParameter(){

      var r = 0

      def f1 = {
        r = r + 2
       }

    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClassSinParameter()
    assert(mc.r == 0)
    val res1 = mc.f1
    assert(mc.r == 2)
    println(s"probando el formateo con ${mc}... esperemos que funcione")

  }

  test("Un case es una clase normal para usos especificos"){

    case class MyCaseClass(a:Int, var b:Int) {
      def f1(a:Int) = a + 1
    }

    // Se puede instanciar de forma normal
    val mcc1 = new MyCaseClass(1, 2)
    assert(mcc1.f1(1) == 2)

    // Se puede instanciar sin new
    val mcc2 = MyCaseClass(1,2)
    assert(mcc2.f1(1) == 2)
    println(s"probando el formateo con ${mcc1}... esperemos que funcione")
    println(mcc2)

    //Que pasa si intentamos println(mcc2) ?

    // Pregunta cuáles son esos casos específicos

  }

  test("Un trait puede tener solo definiciones"){
    trait MyTrait {
      def f1(a:Int):Boolean
    }

    trait MySecondTrait{
      def f2(a:String):Int
    }

    class MyClass extends MyTrait with MySecondTrait{
      override def f1(a:Int) = ???
      override def f2(a:String) = ???
    }

    assertThrows[NotImplementedError]{
      val mc = new MyClass
      mc.f1(1)
    }

  }

  test("Un trait puede tener tambien implementaciones"){
    trait MyTrait {
      def f1(a:Int) = a + 1
    }

    class MyClass extends MyTrait

    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 2)

    object obj extends MyTrait

    val resObj = obj.f1(2)
    assert(resObj == 3)

    case class MyCaseClass() extends MyTrait

    val mcc = MyCaseClass()
    val rescc = mcc.f1(1)
    assert(rescc == 2)
  }

  test("Probando mezclas de rasgos"){
    trait MyTrait {
      val numeroArrugas = 5
    }

    trait MySecondTrait{
      val numeroArrugas = 5
    }

    object obj extends MyTrait with MySecondTrait{

        override val numeroArrugas = 10
        //val numArrugas = super[MySecondTrait].DebeSerUnaFuncion
    }

    assert(obj.numeroArrugas == 10)

  }

  test("Cacharreando"){

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

    class classTest(){

      var r = 0
      var a:Int = 5

      def f1 = {
        r = r + 2
        a + 1 + r
      }

      def f2 = a + 2

      def getA = a
    }

    trait MySecondTrait extends classTest{
      val numeroArrugas = 10
    }

    object o extends MySecondTrait with Calculator
    println(o.getA)
    var resu = o.f1
    println(resu)
    println(o.numeroArrugas)

    case class MyBadClass() extends  MySecondTrait with Calculator
    val mbc = MyBadClass()
    val suma = mbc.suma(5,8)
    println(mbc.getA)
    var result = mbc.f1
    println(result)
    println(mbc.numeroArrugas)
    assert(suma == 13)

    /*case class MyCaseClass() extends Calculator
    val mcc = MyCaseClass()
    val suma = mcc.suma(5,8)
    assert(suma == 13)

    object obj extends Calculator
    val resta = obj.resta(8,5)
    assert(resta == 3)

    class MyClass extends Calculator
    val mc = new MyClass
    val multiplicar = mc.multiplicacion(8,5)
    assert(multiplicar == 40)

    val dividir = obj.division(8,5)
    assert(dividir == 1.6)*/

  }

}
