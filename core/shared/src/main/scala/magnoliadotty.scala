package magnolia
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import scala.compiletime.summonFrom


object Magnolia{

  inline def subtypesOf[Parent, T <: Tuple, Typeclass[_]](tpeName: String, idx: Int)(using m: Mirror.SumOf[Parent]): List[Subtype[Typeclass, Parent]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: ((h, label) *: t) =>
        //todo: better way of getting type names https://github.com/lampepfl/dotty/issues/8739

        val childName = inline constValue[label] match {
          case childName: String => childName
        }

        val headSubtype = Subtype[Typeclass, Parent, Parent](
          name = TypeName(tpeName, childName, Nil),
          idx = idx,
          anns = Array(),
          tc = CallByNeed(summonInline[Typeclass[h]].asInstanceOf[Typeclass[Parent]]),
          isType = m.ordinal(_) == idx,
          asType = a => a
        )

        headSubtype :: subtypesOf[Parent, t, Typeclass](tpeName, idx + 1)
    }

    
  inline def dispatchInternal[T, Typeclass[_]](inline interface: TCInterface[Typeclass])(using m: Mirror.SumOf[T]): Typeclass[T] = {
    val tpeName = constValue[m.MirroredLabel]

    val subtypes = subtypesOf[T, Tuple.Zip[m.MirroredElemTypes, m.MirroredElemLabels], Typeclass](tpeName, 0)

    //todo parent type
    val st: SealedTrait[Typeclass, T]  = new SealedTrait[Typeclass, T](
      TypeName("", tpeName, Nil),
      subtypes.toArray,
      Array()
    )

    interface.dispatch(st)
  }

  inline def parametersOf[Parent, T <: Tuple, Typeclass[_]](idx: Int)(using m: Mirror.ProductOf[Parent]): List[ReadOnlyParam[Typeclass, Parent]] = {
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: ((h, label) *: t) =>
        val paramName = constValue[label] match {
          case paramName: String => paramName
        }

        val param: ReadOnlyParam[Typeclass, Parent] =
          ReadOnlyParam[Typeclass, Parent, h](
            name = paramName,
            idx = idx,
            isRepeated = false, //todo
            typeclassParam = CallByNeed(summonInline[Typeclass[h]]),
            annotationsArrayParam = Array()
          )

        param :: parametersOf[Parent, t, Typeclass](idx + 1)
    }
  }
  
  inline def combineInternal[T, Typeclass[_]](inline interface: TCInterface[Typeclass])(using m: Mirror.ProductOf[T]): Typeclass[T] = {
    val tpeName = constValue[m.MirroredLabel]

    val cc: ReadOnlyCaseClass[Typeclass, T] = 
      new ReadOnlyCaseClass[Typeclass, T](
        //todo parent type
        typeName = TypeName("", tpeName, Nil),
        isObject = false,
        isValueClass = false,
        parametersArray = parametersOf[T, Tuple.Zip[m.MirroredElemTypes, m.MirroredElemLabels], Typeclass](0).toArray,
        annotationsArray = Array()
      ){}

    //todo generic
    interface.combine(cc)
  }

  inline def gen[Typeclass[_], T](inline interface: TCInterface[Typeclass])(using m: Mirror.Of[T]): Typeclass[T] = {
    inline m match {
      case sum: Mirror.SumOf[T] => dispatchInternal[T, Typeclass](interface)(using sum)
      case prod: Mirror.ProductOf[T] => combineInternal[T, Typeclass](interface)(using prod)
    }
  }
}

trait Print[T] {
  def print(t: T): String
}

//ideally we won't have this at all
class TCInterface[Typeclass[_]](
  val combine: [T] => (cc: ReadOnlyCaseClass[Typeclass, T]) => Typeclass[T],
  val dispatch: [T] => (cc: SealedTrait[Typeclass, T]) => Typeclass[T]
)

object Print {
  type Typeclass[T] = Print[T]
  
  def combine[T](ctx: ReadOnlyCaseClass[Print, T]): Print[T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.print(param.dereference(value))
    }
    else {
      ctx.parameters.map { param =>
        param.label + " = " + param.typeclass.print(param.dereference(value))
      }.mkString(s"${ctx.typeName.short}(", ", ", ")")
    }
  }


  def dispatch[T](ctx: SealedTrait[Print, T]): Print[T] = { value =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  inline implicit def derived[T: Mirror.Of]: Print[T] = 
    Magnolia.gen[Print, T](
      new TCInterface[Print](
        [T] => (cc: ReadOnlyCaseClass[Print, T]) => combine(cc),
        [T] => (st: SealedTrait[Print, T]) => dispatch(st),
      )
    )

  implicit val string: Print[String] = a => a
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
  
}

enum MyList derives Print:
  case Cons(h: Int, t: String)
  case End

@main
def run = println(summon[Print[MyList]].print(MyList.Cons(1, "foo")))
// def run = println(summon[Print[MyList]].print(MyList.Cons(1)))
