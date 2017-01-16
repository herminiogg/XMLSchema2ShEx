package es.weso.xmlschema2shex.generation

import es.weso.xmlschema2shex.ast._

/**
  * Created by herminio on 5/10/16.
  */
class CodeGenerator(schema: Schema) {

  val alreadyGeneratedShape = scala.collection.mutable.ListBuffer.empty[ComplexType]
  val generatedShapes = scala.collection.mutable.HashMap.empty[ComplexType, String]

  def generate(): String = {
    generateTypes().mkString("").replaceAll("\"|'", "")
  }

  def generateTypes(): List[String] = {
    for(tag <- schema.tags) yield tag match {
      case c: ComplexType => generateComplexType(c, c.name)
      case e: Element => e.aType.map {
        case c: ComplexType => generateComplexType(c, e.name)
        case _ => ""
      }.getOrElse("")
      case _ => ""
    }
    generatedShapes.values.toList.reverse
  }

  def generateComplexType(complexType: ComplexType, elementName: Option[String]): Unit = {
    val name = complexType.name match {
      case Some(aName) => aName
      case None => elementName match {
        case Some(eName) => eName
        case None => throw new Exception("No name to generate the shape")
      }
    }
    val shape = "<" + name + "> { \n" + generateSequence(complexType.sequence, complexType.attributesElements) + "\n}\n"


    for(element <- complexType.sequence.elements) yield element.aType match {
      case Some(nestedType) => nestedType match {
        case c: ComplexType => generateComplexType(c, Some(name))
        case _ => "" // to implement
      }
      case _ => ""
    }

    if(!alreadyGeneratedShape.contains(complexType)) {
      alreadyGeneratedShape += complexType
      generatedShapes += (complexType -> shape)
    } else {
      alreadyGeneratedShape -= complexType
      generatedShapes -= complexType
      alreadyGeneratedShape += complexType
      generatedShapes += (complexType -> shape)
    }
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
        case c: ComplexType => " @<" + c.name.getOrElse(c.ref.getOrElse(element.name.getOrElse(element.ref.get))) + ">"
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
    val minValue = if(element.minOccurs.isEmpty) Some("1") else element.minOccurs.map(_.replace("'", ""))
    val boundaries = minValue.map(min => {
      val maxValue = if(element.maxOccurs.isEmpty) Some("1") else element.maxOccurs.map(_.replace("'", ""))
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
