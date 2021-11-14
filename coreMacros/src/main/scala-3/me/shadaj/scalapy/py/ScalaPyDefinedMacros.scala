package me.shadaj.scalapy.py

import scala.quoted.*

class Dynamic

object ScalaPyDefinedImpl {
  def register[T: Type](t: Expr[T], bases: Expr[Seq[Dynamic]])(using Quotes): Expr[Dynamic] = {
    import quotes.reflect.*

    def searchImplicit(using Quotes)(typeReprParameter: quotes.reflect.TypeRepr): quotes.reflect.Term = {
      import quotes.reflect.*

      Implicits.search(typeReprParameter) match {
        case success: ImplicitSearchSuccess => {
          success.tree
        }
        case _ => {
          report.throwError(s"There is no implicit for ${typeReprParameter.show}")
        }
      }
    }

    def constructASTforMethodFrom(arg: quotes.reflect.Term) = {
      val applyArgTypeToWriter = Helper.writerTypeRepr.appliedTo(arg.tpe.widen)
      val tree = Apply(Apply(TypeApply(Ref(Helper.methodFromSymbol),List(Inferred(arg.tpe.widen))),List(arg)),
      List(searchImplicit(applyArgTypeToWriter)))
      tree
    }

    val pyPackage = Ref(Symbol.requiredModule("me.shadaj.scalapy.py"))
    val dynamicModule = Ref(Symbol.requiredModule("me.shadaj.scalapy.py.Dynamic"))
    val dynamicClass = TypeIdent(Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic"))
    val anyClass = TypeIdent(Symbol.requiredClass("me.shadaj.scalapy.py.Any"))
    val anyModule = Ref(Symbol.requiredModule("me.shadaj.scalapy.py.Any"))
    val moduleModule = Ref(Symbol.requiredModule("me.shadaj.scalapy.py.Module"))
    val scalaPyNone = Ref(Symbol.requiredMethod("me.shadaj.scalapy.py.None"))
  
    val seq = Ref(Symbol.requiredModule("scala.collection.Seq"))

    val global = Select.unique(dynamicModule, "global")
    val types = Select.overloaded(moduleModule, "apply", Nil, List(Literal(StringConstant("types"))))

    

    def repeatedArgs(args: List[Term]) = 
      Typed(Inlined(None, Nil, Repeated(args.map(constructASTforMethodFrom), anyClass)),Applied(TypeIdent(defn.RepeatedParamClass),List(anyClass)))
    val np = Select.overloaded(moduleModule, "apply", Nil, List(Literal(StringConstant("numpy"))))
    val ndarray = Apply(Select.unique(np, "selectDynamic"), List(Literal(StringConstant("ndarray"))))
    println(ndarray.show)
    // name of the new class
    val name = Literal(StringConstant(TypeTree.of[T].tpe.typeSymbol.name))

    println("CLASS NAME: " + TypeTree.of[T].tpe.typeSymbol.name)

    // bases classes of the new class
    // Dynamic.global.tuple(Seq(Dynamic.global.`object`).toPythonProxy)
    val object1 = Apply(Select.unique(global, "selectDynamic"), List(Literal(StringConstant("object"))))
    // val basesSeq = Apply(TypeApply(Select.unique(seq,"apply"),List(anyClass)),List(repeatedArgs(List(ndarray))))
    val basesSeq = bases match {
      case Seq() => object1
      case _ => bases.asTerm
    }
    // seq.toPythonProxy -> Any.populateWith(implicitly[ConvertableToSeqElem[Seq[Any]]].convertProxy(seq))
    val convert = searchImplicit(Applied(TypeIdent(Symbol.requiredClass("me.shadaj.scalapy.py.ConvertableToSeqElem")), List(Applied(TypeIdent(Symbol.requiredClass("scala.collection.Seq")), List(anyClass)))).tpe)
    val toproxy = Apply(Select.unique(anyModule, "populateWith"), List(Apply(Select.unique(convert, "convertProxy"), List(basesSeq))))
    val basesClasses = Apply(Apply(Select.unique(global, "applyDynamic"), List((Literal(StringConstant("tuple"))))), List(toproxy))

    // execution body of the new class
    val clsexecfunSym = Symbol.newMethod(Symbol.spliceOwner, "clsexec", 
      MethodType(List("ns"))(_ => List(dynamicClass.tpe), _ => anyClass.tpe))

    val methodName = Symbol.spliceOwner.owner.owner.declaredMethods(0).name

    val selectMethod = Select.unique(t.asTerm, methodName)

    // List(Literal(StringConstant("size")), Literal(IntConstant(3)))

    val clsexecfun = DefDef(clsexecfunSym, 
      {
        case List(List(ns @ Ident(_))) => {
          val listOfMethods = Symbol.spliceOwner.owner.owner.declaredMethods.flatMap(method =>{
            
            /* * * * * * * * * *  
              first try to implement getting paramters and using in bracketUpdate  
              * * * * * * * * *  
            */

            // val termParameterSymbols = method.paramSymss.filterNot(_.headOption.exists(_.isType))
            // val parametersAsSymbols = termParameterSymbols.map(_.map(Ref.apply))

            // if parametersAsSymbols.length > 1 then
            //   report.throwError(s"method $methodName has curried parameter lists.")

            // val args = parametersAsSymbols.headOption.toList.flatten

            List(Apply(Select.unique(ns,"bracketUpdate"), List(Expr(method.name).asTerm, Select.unique(t.asTerm, method.name)).map(constructASTforMethodFrom)))
          }) 
          Some(Block(listOfMethods, scalaPyNone))
        }  
      })
    
    val clsexecScala = Block(List(clsexecfun),Closure(Ref(clsexecfunSym), None))
    val clsexec = Apply(Apply(TypeApply(Ref(Helper.methodFromSymbol),List(Inferred(clsexecScala.tpe))),List(clsexecScala)), List(searchImplicit(Helper.writerTypeRepr.appliedTo(clsexecScala.tpe))))

    val newValue = Apply(Apply(Select.unique(types, "applyDynamic"), List(Literal(StringConstant("new_class")))), List(repeatedArgs(List(name, basesClasses, scalaPyNone, clsexec))))
    // val res = Assign(Select.unique(t.asTerm, "rawValue"), Select.unique(newValue, "rawValue"))
    // println(newValue.show)
    newValue.asExprOf[Dynamic]
  }
}