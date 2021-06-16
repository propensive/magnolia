package magnolia

import language.experimental.macros
import scala.annotation.tailrec
import scala.reflect.*

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]):
  def full: String = s"$owner.$short"

object CaseClass:
  trait Param[Typeclass[_], Type](
    val label: String,
    val index: Int,
    val repeated: Boolean,
    val annotations: List[Any],
    val typeAnnotations: List[Any]
  ):
    type PType
    
    def typeclass: Typeclass[PType]
    def deref(param: Type): PType
    def default: Option[PType]
    override def toString: String = s"Param($label)"
  
  object Param:
    def apply[Typeclass[_], T, P](
      name: String,
      idx: Int,
      repeated: Boolean,
      cbn: CallByNeed[Typeclass[P]],
      defaultVal: CallByNeed[Option[P]],
      annotations: List[Any],
      typeAnnotations: List[Any]
    ): Param[Typeclass, T] =
      new CaseClass.Param[Typeclass, T](name, idx, repeated, annotations, typeAnnotations):
        type PType = P
        def default: Option[PType] = defaultVal.value
        def typeclass = cbn.value
        def deref(value: T): P = value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]
  end Param
end CaseClass


abstract class CaseClass[Typeclass[_], Type](
  val typeInfo: TypeInfo,
  val isObject: Boolean,
  val isValueClass: Boolean,
  val params: List[CaseClass.Param[Typeclass, Type]],
  val annotations: List[Any],
  val typeAnnotations: List[Any]
) extends Serializable:

  type Param = CaseClass.Param[Typeclass, Type]
  
  override def toString: String = s"CaseClass(${typeInfo.full}, ${params.mkString(",")})"
  def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): Type
  def constructMonadic[Monad[_]: Monadic, PType: ClassTag](make: Param => Monad[PType]): Monad[Type]
  def constructEither[Err, PType: ClassTag](makeParam: Param => Either[Err, PType]): Either[List[Err], Type]
  def rawConstruct(fieldValues: Seq[Any]): Type

  def param[P](
    name: String,
    idx: Int,
    repeated: Boolean,
    cbn: CallByNeed[Typeclass[P]],
    defaultVal: CallByNeed[Option[P]],
    annotations: List[Any],
    typeAnnotations: List[Any]
  ): Param =
    new CaseClass.Param[Typeclass, Type](name, idx, repeated, annotations, typeAnnotations):
      type PType = P
      def default: Option[PType] = defaultVal.value
      def typeclass = cbn.value
      def deref(value: Type): P = value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]
end CaseClass

case class SealedTrait[Typeclass[_], Type](
  typeInfo: TypeInfo,
  subtypes: List[SealedTrait.Subtype[Typeclass, Type, _]],
  annotations: List[Any],
  typeAnnotations: List[Any]
) extends Serializable:

  type Subtype[S] = SealedTrait.SubtypeValue[Typeclass, Type, S]

  override def toString: String = s"SealedTrait($typeInfo, List[${subtypes.mkString(",")}])"

  def choose[Return](value: Type)(handle: Subtype[_] => Return): Return =
    @tailrec def rec(ix: Int): Return =
      if ix < subtypes.length then
        val sub = subtypes(ix)
        if sub.cast.isDefinedAt(value) then handle(SealedTrait.SubtypeValue(sub, value))
        else rec(ix + 1)
      else throw new IllegalArgumentException(s"The given value `$value` is not a sub type of `$typeInfo`")

    rec(0)

end SealedTrait

object SealedTrait:
  class Subtype[Typeclass[_], Type, SType](
    val typeInfo: TypeInfo,
    val annotations: List[Any],
    val typeAnnotations: List[Any],
    val isObject: Boolean,
    index: Int,
    callByNeed: CallByNeed[Typeclass[SType]],
    isType: Type => Boolean,
    asType: Type => SType & Type
  ) extends PartialFunction[Type, SType & Type], Serializable:
    def typeclass: Typeclass[SType & Type] = callByNeed.value.asInstanceOf[Typeclass[SType & Type]]
    def cast: PartialFunction[Type, SType & Type] = this
    def isDefinedAt(t: Type): Boolean = isType(t)
    def apply(t: Type): SType & Type = asType(t)
    override def toString: String = s"Subtype(${typeInfo.full})"

  object Subtype:
    def apply[Typeclass[_], Type, SType](
      typeInfo: TypeInfo,
      annotations: List[Any],
      typeAnnotations: List[Any],
      isObject: Boolean,
      index: Int,
      callByNeed: CallByNeed[Typeclass[SType]],
      isType: Type => Boolean,
      asType: Type => SType & Type
    ): Subtype[Typeclass, Type, SType] =
      Subtype[Typeclass, Type, SType](
        typeInfo,
        annotations,
        typeAnnotations,
        isObject,
        index,
        callByNeed,
        isType,
        asType
      )

  class SubtypeValue[Typeclass[_], Type, S](val subtype: Subtype[Typeclass, Type, S], v: Type):
    export subtype.{typeclass, typeAnnotations, annotations, cast, typeInfo}
    def value: S & Type = cast(v)

end SealedTrait

object CallByNeed:
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a)

final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable:
  lazy val value: A =
    val result = eval()
    eval = null
    result