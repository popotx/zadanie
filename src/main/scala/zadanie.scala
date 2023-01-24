import scala.reflect.ClassTag

object Main {

  trait PrintableClass[C <: Product] {
    cls: C =>
    def printClass: Unit =
      println(s"Class ${cls.getClass.getSimpleName} with members: ${cls.productElementNames.mkString(",")}")

    protected def printParentInternal[P >: C](implicit ct: ClassTag[P]): Unit =
      println(s"Class ${cls.getClass.getSimpleName} is successor of ${ct.runtimeClass.getSimpleName}")
  }

  trait TraitAddMemberOrNo {
    val additionalMember: String = "additionalMember"
  }

  class ClassAddMemberOrNo(val additionalClassMember: String)


  // 1
  case class A(a: Int, b: String, override val additionalClassMember: String, override val additionalMember: String) extends ClassAddMemberOrNo(additionalClassMember) with TraitAddMemberOrNo with PrintableClass[A] {
    def printParents(): Unit ={
      this.printParentInternal[TraitAddMemberOrNo]
      this.printParentInternal[ClassAddMemberOrNo]
      this.printParentInternal[PrintableClass[A]]
    }
  }


  def main(args: Array[String]): Unit = {
    val a = A(1, "bs", "ACM", "AM")
    // 2
    a.printClass
    // 3
    a.printParents()
  }
}