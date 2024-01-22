package com.herminiogarcia.xmlschema2shex.generation

import com.herminiogarcia.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, ElementsHolder, Schema, SimpleType, Typeable, XSDType, XSNMToken}

/**
  * Created by herminio on 5/10/16.
  */
class CodeGeneratorShExML(schema: Schema) {

  def generate(): String = {
    generateTypes().mkString("").replaceAll("\"|'", "")
  }

  def generateTypes(): List[String] = {
    val output = for(tag <- schema.tags) yield tag match {
      //case c: ComplexType => generateComplexType(c, c.name)
      case e: Element => e.aType.map {
        case c: ComplexType => generateComplexType(c, e.name)
        case _ => List("")
      }.getOrElse(List(""))
      case _ => List("")
    }
    output.flatten.toList
  }

  def generateComplexType(complexType: ComplexType, elementName: Option[String]): List[String] = {
    val name = complexType.name match {
      case Some(aName) => aName
      case None => elementName match {
        case Some(eName) => eName
        case None => throw new Exception("No name to generate the shape")
      }
    }

    val nestedValues = for(element <- complexType.elementsHolder.elements) yield element match {
      case e: Element => List(generateElement(e))
      case _ => List("") // to implement
    }

    val iterator = "ITERATOR " + elementName.getOrElse("") + " </"+elementName.getOrElse("")+"> { \n" +
      "\t" + generateSequence(complexType.elementsHolder, complexType.attributesElements) +
      nestedValues.flatten.mkString("\n") + "\n}\n"


    List(iterator)

    /**if(!alreadyGeneratedShape.contains(complexType)) {
      alreadyGeneratedShape += complexType
      generatedShapes += (complexType -> iterator)
    } else {
      alreadyGeneratedShape -= complexType
      generatedShapes -= complexType
      alreadyGeneratedShape += complexType
      generatedShapes += (complexType -> iterator)
    }*/
  }

  def generateSequence(elementsHolder: ElementsHolder, attributes: List[AttributeElement]): String = {
    val elementsString =
      (for(element <- elementsHolder.elements)
      yield generateElement(element)).mkString("\n")
    val attributesString =
      (for(attribute <- attributes)
      yield generateElement(attribute)).mkString("\n")
    elementsString + "\n" + attributesString
  }

  def generateElement(element: Typeable): String = {
    val elementStart = element.name match {
      case Some(theName) => "FIELD " + theName + " <"+ theName +">"
      case None => element.ref match {
        case Some(ref) => ":" + ref
        case None => ""
      }
    }
    val typeString = element.aType match {
      case Some(theType) => theType match {
        case c: ComplexType => {
          val name = c.name.getOrElse(c.ref.getOrElse(element.name.getOrElse(element.ref.get)))
          generateComplexType(c, Some(name))
        }
        case s: SimpleType => {
          /**s.restriction match {
            case Some(restriction) => restriction.base match {
              case Some(name) => name
              case None => s.name.getOrElse("")
            }
            case None => s.name.getOrElse("")
          }*/
          val name = s.name.getOrElse("")
          "FIELD " + name + " <" + name + ">"
        }
        case x: XSDType => x match {
          case p: XSNMToken => " [\"" + p.value + "\"] "
          case _ => " " + x.name
        }
      }
      case None => ""
    }
    /**val restrictions = element match {
      case t: Typeable => generateRestrictions(t).map(r =>
                if(r.equals("{1}")) "" else " " + r) //implicit boundary

      case _ => Some("")
    }*/
    elementStart + " \n" + typeString // + restrictions.get + " ;"
  }

  /**def generateRestrictions(element: Typeable): Option[String] = {
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
  }*/

}
