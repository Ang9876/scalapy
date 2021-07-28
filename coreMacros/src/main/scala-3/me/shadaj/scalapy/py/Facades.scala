package me.shadaj.scalapy.py


import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.*
import scala.language.dynamics
class native extends StaticAnnotation


object FacadeImpl {
  
  def calleeParamRefs(using Quotes)(callee: quotes.reflect.Symbol): List[List[quotes.reflect.Term]] = {
    import quotes.reflect.*
    val termParamss = callee.paramSymss.filterNot(_.headOption.exists(_.isType))
    termParamss.map(_.map(Ref.apply))
  }

  // def creator[T <: Any](using tagType: quoted.Type[T])(using Quotes): Expr[T] = {
  //   '{new _root_.me.shadaj.scalapy.py.FacadeCreator[$tagType] {
  //     def create(value: _root_.me.shadaj.scalapy.interpreter.PyValue) = new _root_.me.shadaj.scalapy.py.FacadeValueProvider(value) with $tagType {}
  //   }}
  // }
  def creator = ???

  def native_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    val classDynamicSymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic")
    val classReaderSymbol = Symbol.requiredClass("me.shadaj.scalapy.readwrite.Reader")
    val readerTypeRepr = TypeIdent(classReaderSymbol).tpe
    val dynamicTypeRepr = TypeIdent(classDynamicSymbol).tpe
    val applyDynamicTypeToReaderType = readerTypeRepr.appliedTo(dynamicTypeRepr)
    val applyTTypeToReaderType = readerTypeRepr.appliedTo(TypeTree.of[T].tpe)

    println("AST:   " + applyDynamicTypeToReaderType)
    val evidenceForDynamic = Implicits.search(applyDynamicTypeToReaderType) match {
      case success: ImplicitSearchSuccess => {
        println("AST:   " + success.tree)
        success.tree
      }
    }
    val evidenceForTypeT = Implicits.search(applyTTypeToReaderType) match {
        case success: ImplicitSearchSuccess => {
          println("AST:   " + success.tree)
          success.tree
      }
    }
    //classDynamicSymbol.declaredMethods.foreach(println)
    val callee = Symbol.spliceOwner.owner
    val methodName = callee.name
    val refss = calleeParamRefs(callee)

    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten
        
    if (args.isEmpty) {
      val selectDynamicTerm = 
       Apply(//this.as[Dynamic](evidence).selectDynamic(methodName).as[T](evidenceForT)
        TypeApply( //this.as[Dynamic](evidence).selectDynamic(methodName).as[T]
          Select.unique( // this.as[Dynamic](evidence).selectDynamic(methodName).as
            Apply( // this.as[Dynamic](evidence).selectDynamic(methodName)
              Select.unique( // this.as[Dynamic](evidence).selectDynamic
                Apply(      // this.as[Dynamic](evidence)
                  TypeApply(  // this.as[Dynamic]
                    Select.unique( // this.as
                      resolveThis,  // this
                      "as"
                    ),
                    List(TypeIdent(classDynamicSymbol))
                  ),
                  List(evidenceForDynamic)
                ),  
                "selectDynamic"
              ),
              //List(Literal(StringConstant(methodName)))
              List(Expr(methodName).asTerm)
            ),
            "as"
          ),
          List(TypeTree.of[T]) 
        ),
        List(evidenceForTypeT)
       ) 
        
      println("IS EXPR: " + selectDynamicTerm)
      selectDynamicTerm.asExprOf[T]
    }
    else {
      val applyDynamicTerm = 
       Apply(//  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as[T](evidenceForT)
        TypeApply(   //  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as[T]
          Select.unique(  //  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as
            Apply(    // this.as[Dynamic](evidence).applyDynamic(methodName)(parameters)
              Apply(  // this.as[Dynamic](evidence).applyDynamic(methodName)
                Select.unique( // this.as[Dynamic](evidence).applyDynamic
                  Apply(      // this.as[Dynamic](evidence)
                    TypeApply(  // this.as[Dynamic]
                      Select.unique( // this.as
                        resolveThis,  // this
                        "as"
                      ),
                      List(TypeIdent(classDynamicSymbol))
                    ),
                    List(evidenceForDynamic)
                  ),
                  "applyDynamic"
                ),
                List(Expr(methodName).asTerm)
              ),
              List(Varargs(args.map(_.asExpr)).asTerm)
            ),
            "as"
          ),
          List(TypeTree.of[T])         
        ),
        List(evidenceForTypeT)
       ) 
      applyDynamicTerm.asExprOf[T]
    }
  }

  def resolveThis(using Quotes): quotes.reflect.Term =
    import quotes.reflect.*
    var sym = Symbol.spliceOwner  // symbol of method where the macro is expanded
    while sym != null && !sym.isClassDef do
      sym = sym.owner  // owner of a symbol is what encloses it: e.g. enclosing method or enclosing class
    This(sym)

  def native_named_impl = ???
}
