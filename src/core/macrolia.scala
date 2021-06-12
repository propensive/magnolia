package magnolia

import scala.quoted.*
import scala.reflect.*
import scala.compiletime.*
import Macro.*

trait MacroDerivation[TypeClass[_]]:
  type Typeclass[T] = TypeClass[T]
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]

  transparent inline def derived[T]: Typeclass[T] =
    if isProduct[T] then
      val params: List[CaseClass.Param[Typeclass, T]] = MacroDerivation.getParams[Typeclass, T]
      val cc = new CaseClass[Typeclass, T](
        typeInfo[T],
        isObject[T],
        isValueClass[T],
        IArray.from(params),
        IArray.from(anns[T]),
        IArray[Any](typeAnns[T]*)
      ) {
        def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): T = ???
        def rawConstruct(fieldValues: Seq[Any]): T = ???
        def constructEither[Err, PType: ClassTag](makeParam: Param => Either[Err, PType]): Either[List[Err], T] = ???
        def constructMonadic[M[_]: Monadic, PType: ClassTag](makeParam: Param => M[PType]): M[T] = ???
      }
      join(cc)
    else if isSum[T] then
      ???
    else
      ???

end MacroDerivation

object MacroDerivation:

  inline def getParams[Typeclass[_], T]: List[CaseClass.Param[Typeclass, T]] = ${getParamsImpl[Typeclass, T]}

  def getParamsImpl[Typeclass[_]: Type, T: Type](using Quotes): Expr[List[CaseClass.Param[Typeclass, T]]] =
    import quotes.reflect.*

    def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match
      case a: AnnotatedType =>
        a.annotation.tpe match
          case tr: TypeRef => tr.name == "Repeated"
          case _           => false
      case _ => false

    def getTypeAnnotations(t: TypeRepr): List[Term] = t match
      case AnnotatedType(inner, ann) => ann :: getTypeAnnotations(inner)
      case _                         => Nil

    def filterAnnotations(annotations: List[Term]): List[Term] =
      annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
          a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
      }

    val typeSymbol = TypeRepr.of[T].typeSymbol

    val paramObj = '{CaseClass.Param}.asTerm
    val paramConstrSymbol = TypeRepr.of[CaseClass.Param.type].termSymbol.declaredMethod("apply").head

    val annotations = paramAnns[T].to(Map)
    val typeAnnotations = paramTypeAnns[T].to(Map)

    def termFromInlinedTypeApplyUnsafe(t: Term): Term = t match
      case Inlined(_, _, TypeApply(term, _)) => term

    Expr.ofList {
      typeSymbol.caseFields.zipWithIndex.collect {
        case (paramSymbol: Symbol, idx: Int) =>
          val valdef: ValDef = paramSymbol.tree.asInstanceOf[ValDef]
          val paramTypeTree = valdef.tpt
          val paramTypeclassTree = Applied(TypeTree.of[Typeclass], List(paramTypeTree))
          val summonInlineTerm = termFromInlinedTypeApplyUnsafe('{scala.compiletime.summonInline}.asTerm)
          val summonInlineApp = TypeApply(summonInlineTerm, List(paramTypeclassTree))
          val callByNeedTerm = '{CaseClass.Param}.asTerm
          val callByNeedApply = TypeRepr.of[CallByNeed.type].termSymbol.declaredMethod("apply").head
          val instance = Apply(TypeApply(Select(callByNeedTerm, callByNeedApply), List(paramTypeclassTree)), List(summonInlineApp))
          Apply(
            fun = TypeApply(
              fun = Select(
                qualifier = paramObj,
                symbol = paramConstrSymbol
              ),
              args = List(TypeTree.of[Typeclass], TypeTree.of[T], paramTypeTree)
            ),
            args = List(
              /*name =*/ Expr(paramSymbol.name).asTerm,
              /*idx =*/ Expr(idx).asTerm,
              /*repeated =*/ Expr(isRepeated(paramTypeTree.tpe)).asTerm,
              /*cbn =*/ instance, //TODO FIX ME
              /*defaultVal =*/ '{CallByNeed(None)}.asTerm,
              /*annotations =*/ Expr.ofList(filterAnnotations(paramSymbol.annotations).toSeq.map(_.asExpr)).asTerm,
              /*typeAnnotations =*/ Expr.ofList(filterAnnotations(getTypeAnnotations(paramTypeTree.tpe)).toSeq.map(_.asExpr)).asTerm
            )
          ).asExprOf[CaseClass.Param[Typeclass, T]]
      }
    }

  end getParamsImpl

end MacroDerivation