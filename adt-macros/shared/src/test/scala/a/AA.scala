package aa

//import japgolly.microlibs.adt_macros.AdtMacros

sealed abstract class MonoS1
object MonoS1 {
  case object A extends MonoS1
//   val Values = AdtMacros.adtValues[MonoS1] // SI-7046
//   val ValuesM = AdtMacros.adtValuesManual[MonoS1](A) // SI-7046
}

sealed trait MonoS3
object MonoS3 {
  case object A extends MonoS3
  case object B extends MonoS3
  case object C extends MonoS3
//  // val Values = AdtMacros.adtValues[MonoS3] // SI-7046
//  // val ValuesM = AdtMacros.adtValuesManual[MonoS3](A, B, C) // SI-7046
}

trait Unsealed
object Unsealed {
  case object A extends Unsealed
}

sealed abstract class MonoD1
object MonoD1 {
  case class I(i: Int) extends MonoD1
}

sealed abstract class MonoD2
object MonoD2 {
  case object A extends MonoD2
  case class B(i: Boolean) extends MonoD2
}

sealed abstract class MonoD
object MonoD {
  case class A() extends MonoD
  case class B(int: Int) extends MonoD
  case object C extends MonoD
  case class D(a: MonoD, b: MonoD) extends MonoD
}

sealed trait MonoSub
object MonoSub {
  case object A extends MonoSub
  sealed abstract class B extends MonoSub
  case object B1 extends B
  case object B2 extends B
}
