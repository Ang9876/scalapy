package me.shadaj.scalapy.py

trait ScalaPyDefined {
  inline def registerScalaPyDefined[T](t: T, bases: Dynamic*): Unit = 
    t.asInstanceOf[Any].`type` = register[T](t, bases: _*)
    
  inline def register[T](t: T, bases: Dynamic*): Dynamic = ${ScalaPyDefinedImpl.register[T]('t, 'bases)}
}
