package me.shadaj.scalapy.py

import scala.collection.mutable

import org.scalatest.funsuite.AnyFunSuite

import me.shadaj.scalapy.interpreter.PyValue
import me.shadaj.scalapy.readwrite.{Reader, Writer}

import scala.quoted.*

@native class NdArray extends Any {
  def size: Int = native
  def reshape(args: Int*): NdArray = this.as[Dynamic].reshape(args.toPythonProxy).as[NdArray]
}

class MyArray extends NdArray {
  override def size: Int = 7
  registerScalaPyDefined(this, module("numpy").ndarray)
}

class ScalaA extends Any {
  def size: Int = 2
  def getInteger: Int = 3
  registerScalaPyDefined(this)
}

class ScalaPyDefinedTest extends AnyFunSuite {
  test("ScalaPy Defined Test") {
    local {
      val np = module("numpy")
      
      val a = np.ndarray(Seq(1, 2).toPythonProxy).as[NdArray]

      val b12 = MyArray()    
      b12.initRawValue(Seq(1, 2).toPythonProxy)
      println(b12)
      val b21: MyArray = b12.reshape(2, 1).as[MyArray]
      println(b21)
      println(np.apply_along_axis(((x: NdArray) => x.as[Dynamic].size), 1, b12))
      println(np.apply_along_axis(((x: MyArray) => x.as[Dynamic].size), 1, b21))

      val scalaa = new ScalaA
      scalaa.initRawValue()
      println(scalaa.size)
      
      assert(scalaa.as[Dynamic].size.as[Int] == 2)
      assert(scalaa.as[Dynamic].getInteger.as[Int] == 3)      

      // variable is a problem
    }
  }
}
