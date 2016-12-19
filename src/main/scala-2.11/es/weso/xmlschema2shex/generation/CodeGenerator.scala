package es.weso.xmlschema2shex.generation

import es.weso.xmlschema2shex.ast._

/**
  * Created by herminio on 5/10/16.
  */
class CodeGenerator(schema: Schema) {

  val alreadyGeneratedShape = scala.collection.mutable.ListBuffer.empty[ComplexType]

  def generate(): String = {
    generateTypes().mkString("").replaceAll("\"", "")
  }

  def generateTypes(): List[String] = {
    for(tag <- schema.tags) yield tag match {
      case c: ComplexType => generateComplexType(c)
      case _ => ""
    }
  }

  def generateComplexType(complexType: ComplexType): String = {
    val name = complexType.name match {
      case Some(aName) => aName
      case None =>  "dummy"
    }
    val shape =
      if(!alreadyGeneratedShape.contains(complexType))
        "<" + name + "> { \n" + generateSequence(complexType.sequence, complexType.attributesElements) + "\n}\n"
      else ""
    alreadyGeneratedShape += complexType
    val nestedShapes = for(element <- complexType.sequence.elements) yield element.aType match {
      case Some(nestedType) => nestedType match {
        case c: ComplexType => generateComplexType(c)
        case _ => "" // to implement
      }
      case _ => ""
    }
    shape + nestedShapes.mkString("")
  }

  def generateSequence(sequence: Sequence, attributes: List[AttributeElement]): String = {
    val elementsString =
      (for(element <- sequence.elements)
      yield generateElement(element)).mkString("\n")
    val attributesString =
      (for(attribute <- attributes)
      yield generateElement(attribute)).mkString("\n")
    elementsString + "\n" + attributesString
  }

  def generateElement(element: Typeable): String = {
    val elementStart = element.name match {
      case Some(theName) => ":" + theName
      case None => element.ref match {
        case Some(ref) => ":" + ref
        case None => None
      }
    }
    val typeString = element.aType match {
      case Some(theType) => theType match {
        case c: ComplexType => " @<" + c.name.get + ">"
        case s: SimpleType => if(s.name.isDefined) " " + s.name.get else "fail"
        case x: XSDType => x match {
          case p: XSNMToken => " [\"" + p.value + "\"] "
          case _ => " " + x.name
        }
      }
      case None => ""
    }
    val restrictions = element match {
      case t: Typeable => generateRestrictions(t).map(r =>
                if(r.equals("{1}")) "" else " " + r) //implicit boundary

      case _ => Some("")
    }
    elementStart + typeString + restrictions.get + " ;"
  }

  def generateRestrictions(element: Typeable): Option[String] = {
    val minValue = if(element.minOccurs.isEmpty) Some("1") else element.minOccurs
    val boundaries = minValue.map(min => {
      val maxValue = if(element.maxOccurs.isEmpty) Some("1") else element.maxOccurs
      maxValue.map(max => {
        if(min.toInt == 0 && max.equals("unbounded")) "*"
        else if(min.toInt == 1 && max.equals("unbounded")) "+"
        else if(min.toInt == 0 && max.toInt == 1) "?"
        else if(min.toInt == max.toInt) "{" + min.toInt + "}"
        else "{" + min.toInt + ", " + max.toInt + "}"
      }).get
    }).getOrElse("")
    val pattern = element.pattern.map("PATTERN " + _).getOrElse("")
    Some(List(boundaries, pattern).mkString(" "))
  }

}
