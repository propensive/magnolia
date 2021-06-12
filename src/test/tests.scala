package magnolia.tests

import magnolia.examples.*
import magnolia.TypeInfo

import java.time.LocalDate

import scala.annotation.StaticAnnotation

type ShowStr = [X] =>> Show[String, X ]

sealed trait Tree[+T]
case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Path[+A]
case class Destination[+A](value: A) extends Path[A]
case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
case class OffRoad[+A](path: Option[Path[A]]) extends Path[A]

sealed trait Entity

case class Company(name: String) extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)

class Length(val value: Int) extends AnyVal

case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: String)
case class Fruit(name: String)

case class Item(name: String, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Orange extends Color
case object Pink extends Color

sealed trait Sport
case object Boxing extends Sport
case class Soccer(players: Int) extends Sport

case class MyAnnotation(order: Int) extends StaticAnnotation
case class MyTypeAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
  @MyAnnotation(1) p1: String @MyTypeAnnotation(0),
  @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
) extends AttributeParent @MyTypeAnnotation(2)

case class `%%`(`/`: Int, `#`: String)

case class Param(a: String, b: String)
case class TestEntry(param: Param)
object TestEntry {
  def apply(): TestEntry = TestEntry(Param("", ""))

  def apply(a: String)(using b: Int): TestEntry = TestEntry(Param(a, b.toString))

  def apply(a: String, b: String): TestEntry = TestEntry(Param(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S) extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

sealed trait Box[+A]
case class SimpleBox[+A](value: A) extends Box[A]
case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

case class Account(id: String, emails: String*)

case class Portfolio(companies: Company*)

case class Recursive(children: Seq[Recursive])

// This tests compilation.
// class GenericCsv[A: Csv]
// object ParamCsv extends GenericCsv[Param]


class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: String) extends AnyVal
final case class ServiceName2(value: String)

@MyAnnotation(0)
@SuppressWarnings(Array("deprecation"))
@JavaExampleAnnotation(description = "Some model")
case class MyDto(foo: String, bar: Int)

@SerialVersionUID(42) case class Schedule(events: Seq[Event])
case class Event(date: LocalDate)

case class RPerson(age: Int, name: String, children: Seq[RPerson])
case class GPerson(children: Seq[RPerson])

case class ProtectedCons protected (name: String)
object ProtectedCons {
  def apply(firstName: String, familyName: String): ProtectedCons =
    new ProtectedCons(firstName + " " + familyName)
}

case class PrivateCons private (name: String)
object PrivateCons {
  def apply(firstName: String, familyName: String): PrivateCons =
    new PrivateCons(firstName + " " + familyName)
}

// class PrivateValueClass private (val value: Int) extends AnyVal
// object PrivateValueClass {
//   def apply(l: Int) = new PrivateValueClass(l)
//   implicit val show: Show[String, PrivateValueClass] = Show.derived
// }

case class KArray(value: List[KArray])
case class Wrapper(v: Option[KArray])

case class VeryLong(
  p1: String,
  p2: String,
  p3: String,
  p4: String,
  p5: String,
  p6: String,
  p7: String,
  p8: String,
  p9: String,
  p10: String,
  p11: String,
  p12: String,
  p13: String,
  p14: String,
  p15: String,
  p16: String,
  p17: String,
  p18: String,
  p19: String,
  p20: String,
  p21: String,
  p22: String,
  p23: String
)

case class Character(id: Character.Id)
object Character {
  trait Tag extends Any
  type Id = Long with Tag
}

case class AnotherCharacter(id: AnotherCharacter.Id)
object AnotherCharacter {
  trait Tag extends Any
  type Id = Long with Tag

  given idShow: Show[String, Id] = _.toString
}

final case class Abc(
  private val a: Int,
  private val b: Long,
  c: String
)

sealed trait Covariant[+A]
sealed trait Contravariant[-A]
sealed trait Exactly[A] extends Covariant[A] with Contravariant[A]

object Exactly {
  case object Any extends Exactly[Any]
  case class Custom[A](value: A) extends Exactly[A]
  case object Int extends Exactly[Int]
  case object Nothing extends Exactly[Nothing]
  case object String extends Exactly[String]
}

case class ParamsWithDefault(a: Int = 3, b: Int = 4)
case class ParamsWithDefaultGeneric[A, B](a: A = "A", b: B = "B")

sealed trait Parent
trait BadChild extends Parent // escape hatch!
sealed trait GoodChild extends Parent
final case class Huey(height: Int) extends GoodChild
class Dewey(val height: Int) extends GoodChild
final case class Louie(height: Int) extends BadChild

object Obj1:
  object Obj2:
    case class NestedInObjects(i: Int)

sealed trait Y
case object A extends Y
case class B(s: String) extends Y

class Tests extends munit.FunSuite {

  test("work in some way") {
    case class ASDF(ghj: String)
    val res = Print.derived[ASDF].print(ASDF("XD"))
    assertEquals(res, """ASDF(ghj=XD)""")
  }

  // test("construct a Show product instance with alternative apply functions") {
  //   val res = Show.derived[TestEntry].show(TestEntry("a", "b"))
  //   assertEquals(res, """TestEntry(param=Param(a=a,b=b))""")
  // }

  // test("construct a Show product instance") {
  //   val res = Show.derived[Person].show(Person("John Smith", 34))
  //   assertEquals(res, """Person(name=John Smith,age=34)""")
  // }

  // test("construct a Show coproduct instance") {
  //   val res = Show.derived[Person].show(Person("John Smith", 34))
  //   assertEquals(res, "Person(name=John Smith,age=34)")
  // }

  // test("construct a Show instance for product with partially private fields") {
  //   val res = Show.derived[Abc].show(Abc(12, 54, "pm"))
  //   assertEquals(res, "Abc(a=12,b=54L,c=pm)")
  // }

  // test("construct a Show instance for a product with multiple default values") {
  //   val res = Show.derived[ParamsWithDefault].show(ParamsWithDefault())
  //   assertEquals(res, "ParamsWithDefault(a=3,b=4)")
  // }

  // test("local implicit beats Magnolia") {
  //   given showPerson: Show[String, Person] = _ => "nobody"
  //   val res = summon[Show[String, Address]].show(Address("Home", Person("John Smith", 44)))
  //   assertEquals(res, "Address(line1=Home,occupant=nobody)")
  // }

  // test("even low-priority implicit beats Magnolia for nested case") {
  //   val res = summon[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
  //   assertEquals(res, "Lunchbox(fruit=apple,drink=lemonade)")
  // }

  // test("low-priority implicit beats Magnolia when not nested") {
  //   val res = summon[Show[String, Fruit]].show(Fruit("apple"))
  //   assertEquals(res, "apple")
  // }

  // test("low-priority implicit beats Magnolia when chained") {
  //   val res = summon[Show[String, FruitBasket]].show(FruitBasket(Fruit("apple"), Fruit("banana")))
  //   assertEquals(res, "FruitBasket(fruits=[apple,banana])")
  // }

  // test("typeclass implicit scope has lower priority than ADT implicit scope") {
  //   val res = summon[Show[String, Fruit]].show(Fruit("apple"))
  //   assertEquals(res, "apple")
  // }

  // test("capture attributes against params") {
  //   val res = summon[Show[String, Attributed]].show(Attributed("xyz", 100))
  //   assertEquals(res, "Attributed{MyAnnotation(0)}{MyTypeAnnotation(2)}(p1{MyAnnotation(1)}{MyTypeAnnotation(0)}=xyz,p2{MyAnnotation(2)}{MyTypeAnnotation(1)}=100)")
  // }

  // test("test equality false") {
  //   val res = Eq.derived[Entity].equal(Person("John Smith", 34), Person("", 0))
  //   assert(!res)
  // }

  // test("test equality true") {
  //   val res = Eq.derived[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
  //   assert(res)
  // }

  // test("test branch equality true") {
  //   val res = Eq.derived[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
  //   assert(res)
  // }

  // test("construct a default value") {
  //   val res = HasDefault.derived[Entity].defaultValue
  //   assertEquals(res, Right(Company("")))
  // }

  // test("serialize a Leaf") {
  //   val res = implicitly[Show[String, Leaf[String]]].show(Leaf("testing"))
  //   assertEquals(res, "Leaf[String](value=testing)")
  // }

  // test("serialize case object") {
  //   val res = summon[Show[String, Red.type]].show(Red)
  //   assertEquals(res, "Red()")
  // }

  // test("serialize self recursive type") {
  //   val res = summon[Show[String, GPerson]].show(GPerson(Nil))
  //   assertEquals(res, "GPerson(children=[])")
  // }

  // test("serialize case object as a sealed trait") {
  //   val res = summon[Show[String, Color]].show(Blue)
  //   assertEquals(res, "Blue()")
  // }

  // test("serialize case class with protected constructor") {
  //   val res = ProtectedCons.show.show(ProtectedCons("dada", "phil"))
  //   assertEquals(res, "ProtectedCons(name=dada phil)")
  // }

  // test("serialize case class with accessible private constructor") {
  //   val res = PrivateCons.show.show(PrivateCons("dada", "phil"))
  //   assertEquals(res, "PrivateCons(name=dada phil)")
  // }

  // test("read-only typeclass can serialize case class with inaccessible private constructor") {
  //   val res = summon[Print[PrivateCons]].print(PrivateCons("dada", "phil"))
  //   assertEquals(res, "PrivateCons(dada phil)")
  // }

  // test("read-only typeclass can serialize case class with protected constructor") {
  //   val res = summon[Print[ProtectedCons]].print(ProtectedCons("dada", "phil"))
  //   assertEquals(res, "ProtectedCons(dada phil)")
  // }

  // test("decode a company") {
  //   val res = Decoder.derived[Company].decode("""Company(name=Acme Inc)""")
  //   assertEquals(res, Company("Acme Inc"))
  // }

  // test("decode a Person as an Entity") {
  //   val res = summon[Decoder[Entity]].decode("""magnolia.tests.Person(name=John Smith,age=32)""")
  //   assertEquals(res, Person("John Smith", 32))
  // }

  // test("decode a product nested in objects") {
  //   import Obj1.Obj2._
  //   val res = summon[Decoder[NestedInObjects]].decode("""magnolia.tests.Obj1.Obj2.NestedInObjects(i=42)""")
  //   assertEquals(res, NestedInObjects(42))
  // }

  // test("decode a nested product") {
  //   val res = summon[Decoder[Address]].decode(
  //     """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
  //   )
  //   assertEquals(res, Address("53 High Street", Person("Richard Jones", 44)))
  // }

  // test("typenames and labels are not encoded") {
  //   val res = summon[Show[String, `%%`]].show(`%%`(1, "two"))
  //   assertEquals(res, "%%(/=1,#=two)")
  // }

  // val tupleDerivation = summon[Show[String, (Int, String)]]

  // test("serialize a tuple") {
  //   val res = tupleDerivation.show((42, "Hello World"))
  //   assertEquals(res, "Tuple2[Int,String](_1=42,_2=Hello World)")
  // }

  // // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
  // test("show a Politician with covariant lobby") {
  //   val res = Show.derived[Politician[String]].show(Corrupt("wall", Seq(Company("Alice Inc"))))
  //   assertEquals(res, "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")
  // }

  // //test("patch a Person via a Patcher[Entity]") {
  // //  val person = Person("Bob", 42)
  // //  summon[Patcher[Entity]].patch(person, Seq(null, 21))
  // //}.assert(_ == Person("Bob", 21))

  // test("show an Account") {
  //   val res = Show.derived[Account].show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
  //   assertEquals(res, "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")
  // }

  // test("construct a default Account") {
  //   val res = HasDefault.derived[Account].defaultValue
  //   assertEquals(res, Right(Account("")))
  // }

  // test("construct a failed NoDefault") {
  //   val res = HasDefault.derived[NoDefault].defaultValue
  //   assertEquals(res, Left("truth is a lie"))
  // }

  // test("show a Portfolio of Companies") {
  //   val res = Show.derived[Portfolio].show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
  //   assertEquals(res, "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")
  // }

  // // test("show a List[Int]") {
  // //   given [T: [X] =>> Show[String, X]] : Show[String, List[T]] = Show.derived

  // //   Show.derived[List[Int]].show(List(1, 2, 3))
  // //.assert(_ == "::[Int](head=1,tl=::[Int](head=2,tl=::[Int](head=3,tl=Nil())))")
  // // }

  // test("sealed trait typeName should be complete and unchanged") {
  //   val res = TypeNameInfo.derived[Color].name
  //   assertEquals(res.full, "magnolia.tests.Color")
  // }

  // test("sealed trait subtypes should be ordered") {
  //   val res = TypeNameInfo.derived[Color].subtypeNames.map(_.short)
  //   assertEquals(res, Seq("Red", "Green", "Blue", "Orange", "Pink"))
  // }

  // test("sealed trait subtypes should detect isObject") {
  //   val subtypeIsObjects = ObjectInfo.derived[Sport].subtypeIsObject
  //   assertEquals(subtypeIsObjects, Seq(true, false))
  // }

  // test("sealed trait enumeration should detect isObject") {
  //   val subtypeIsObjects = ObjectInfo.derived[Color].subtypeIsObject
  //   assertEquals(subtypeIsObjects, Seq(true, true, true, true, true))
  // }

  // test("case class typeName should be complete and unchanged") {
  //   given stringTypeName: TypeNameInfo[String] with {
  //     def name = ???

  //     def subtypeNames = ???
  //   }
  //   val res = TypeNameInfo.derived[Fruit].name
  //   assertEquals(res.full, "magnolia.tests.Fruit")
  // }

  // test("show a recursive case class") {
  //   val res = Show.derived[Recursive].show(Recursive(Seq(Recursive(Nil))))
  //   assertEquals(res, "Recursive(children=[Recursive(children=[])])")
  // }

  // test("manually derive a recursive case class instance") {
  //   val res = Recursive.showRecursive.show(Recursive(Seq(Recursive(Nil))))
  //   assertEquals(res, "Recursive(children=[Recursive(children=[])])")
  // }

  // test("show underivable type with fallback") {
  //   val res = summon[TypeNameInfo[NotDerivable]].name
  //   assertEquals(res, TypeInfo("", "Unknown Type", Seq.empty))
  // }

  // test("equality of Wrapper") {
  //   val res = Eq.derived[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
  //   assert(!res)
  // }

  // test("very long") {
  //   val vl =
  //     VeryLong("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15",
  //         "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23")
  //   val res = Eq.derived[VeryLong].equal(vl, vl)
  //   assert(res)
  // }

  // test("construct a semi print for sealed hierarchy") {
  //   val res = SemiPrint.derived[Y].print(A)
  //   assertEquals(res, "A()")
  // }

  // test("construct a semi print for recursive hierarchy") {
  //   given instance: SemiPrint[Recursive] = SemiPrint.derived
  //   val res = instance.print(Recursive(Seq(Recursive(Seq.empty))))

  //   assertEquals(res, "Recursive(Recursive())")
  // }

  // test("not find a given for semi print") {
  //   val res = compileErrors("""summon[SemiPrint[Y]].print(A)""")
  //   assert(res.nonEmpty)
  // }
}