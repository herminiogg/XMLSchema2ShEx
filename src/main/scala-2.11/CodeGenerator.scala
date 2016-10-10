
/**
  * Created by herminio on 5/10/16.
  */
class CodeGenerator(schema: Schema) {

  def generate(): String = {
    generateTypes().mkString("").replaceAll("\"", "")
  }

  def generateTypes(): List[String] = {
    for(tag <- schema.tags) yield tag match {
      case c: ComplexType => generateComplexType(c, Nil)
      case _ => ""
    }
  }

  def generateComplexType(complexType: ComplexType, alreadyGeneratedShape: List[ComplexType]): String = {
    val name = complexType.name match {
      case Some(aName) => aName
      case None =>  "dummy"
    }
    val shape =
      if(!alreadyGeneratedShape.contains(complexType))
        "<" + name + "> { \n" + generateSequence(complexType.sequence) + "\n}\n"
      else ""
    val nestedShapes = for(element <- complexType.sequence.elements) yield element.aType match {
      case Some(nestedType) => nestedType match {
        case c: ComplexType => generateComplexType(c, alreadyGeneratedShape :+ c)
        case _ => "" // to implement
      }
      case _ => ""
    }
    shape + nestedShapes.mkString(" ")
  }

  def generateSequence(sequence: Sequence): String = {
    (for(element <- sequence.elements)
      yield generateElement(element)).mkString("\n")
  }

  def generateElement(element: Element): String = {
    val elementStart = element.name match {
      case Some(theName) => ":" + theName
      case None => "" //name unknown
    }
    val typeString = element.aType match {
      case Some(theType) => theType match {
        case c: ComplexType => " @<" + c.name.get + ">"
        case s: SimpleType => if(s.name.isDefined) " " + s.name.get else "fail"
        case x: XSDType => " " + x.name
      }
      case None => ""
    }
    elementStart + typeString + " ;"
  }

}
