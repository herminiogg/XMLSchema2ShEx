package com.herminiogarcia.xmlschema2shex.generation

import com.herminiogarcia.shexml.ast.{Action, DataTypeLiteral, ExpOrVar, ObjectElement, Predicate, PredicateObject, ShExML, Shape, ShapeLink, ShapeVar, Var}
import com.herminiogarcia.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, ElementsHolder, Schema, SimpleType, Typeable, XSDType, XSNMToken}

/**
  * Created by herminio on 5/10/16.
  */
class XMLSchema2ShExMLShapesGeneration(schema: Schema) extends NameNormalizator {

  val generatedShapes = scala.collection.mutable.ListBuffer.empty[Shape]

  def generate(): ShExML = {
    ShExML(Nil, Nil, generateTypes())
  }

  def generateTypes(): List[Shape] = {
    for(tag <- schema.tags) yield tag match {
      case e: Element => e.aType.map {
        case c: ComplexType => generateComplexType(c, e.name, "")
        case _ => ""
      }.getOrElse("")
      case _ => ""
    }
    generatedShapes.toList.reverse
  }

  def generateComplexType(complexType: ComplexType, elementName: Option[String], precedingNavigation: String): Unit = {
    val name = elementName match {
      case Some(aName) => aName
      case None => complexType.name match {
        case Some(eName) => eName
        case None => throw new Exception("No name to generate the shape")
      }
    }
    val shapeVar = ShapeVar(getDefaultPrefix() + normalizeName(name))
    val prefix = getDefaultPrefix()
    val precedingNavigationString =
      if(precedingNavigation.isEmpty) "exp" else precedingNavigation + "." + normalizeName(name)
    val predicateObjects = generateSequence(complexType.elementsHolder, complexType.attributesElements, precedingNavigationString)
    val action = predicateObjects.find(po => po.predicate.`extension`.matches("[a-zA-Z0-9]*id[a-zA-Z0-9]*")
        && po.objectOrShapeLink.isInstanceOf[ObjectElement]) match {
      case Some(id) => id.objectOrShapeLink.asInstanceOf[ObjectElement].action.orNull
      case None => Var("subjectAutoincrementId")
    }
    val actionObject = Action(prefix, action, None)
    val shape = Shape(shapeVar, actionObject, predicateObjects, None)

    for(element <- complexType.elementsHolder.elements) yield element.aType match {
      case Some(nestedType) => nestedType match {
        case c: ComplexType => {
          val name = if(element.name.isDefined) element.name else element.ref
          generateComplexType(c, name, precedingNavigationString)
        }
        case _ => "" // to implement
      }
      case _ => ""
    }
    if(!generatedShapes.exists(s => s.shapeName.name == shape.shapeName.name))
      generatedShapes += shape
    else {
      val index = generatedShapes.indexOf(generatedShapes.find(s => s.shapeName.name == shape.shapeName.name).get)
      generatedShapes.update(index, shape)
    }
  }

  def generateSequence(elementsHolder: ElementsHolder, attributes: List[AttributeElement], precedingActionNavigation: String): List[PredicateObject] = {
    val elementsPredicateObjects =
      (for(element <- elementsHolder.elements)
      yield generateElement(element, precedingActionNavigation))
    val attributesPredicateObjects =
      (for(attribute <- attributes)
      yield generateElement(attribute, precedingActionNavigation))
    elementsPredicateObjects ::: attributesPredicateObjects
  }

  def generateElement(element: Typeable, precedingActionNavigation: String): PredicateObject = {
    val predicate = element.name match {
      case Some(theName) => Predicate(getDefaultPrefix(), normalizeName(theName))
      case None => element.ref match {
        case Some(ref) => Predicate(getDefaultPrefix(), normalizeName(ref))
      }
    }
    val objectOrShapeLink = element.aType match {
      case Some(theType) => {
        val varString =
          if(precedingActionNavigation.isEmpty) Var(normalizeName(predicate.`extension`))
          else Var(precedingActionNavigation + "." + normalizeName(predicate.`extension`))
        val action = Some(varString)
        theType match {
          case c: ComplexType =>
            val name = element.name.getOrElse(element.ref.getOrElse(c.name.getOrElse(c.ref.get)))
            ShapeLink(ShapeVar(getDefaultPrefix() + normalizeName(name)))
          case s: SimpleType => {
            s.restriction match {
              case Some(restriction) => restriction.base match {
                case Some(name) =>
                  ObjectElement("", action, None, None, None, Some(DataTypeLiteral(normalizeName(name))), None, None)
                case None => ObjectElement("", action, None, None, None, s.name.map(DataTypeLiteral.apply), None, None)
              }
              case None => ObjectElement("", action, None, None, None, s.name.map(DataTypeLiteral.apply), None, None)
            }
          }
          case x: XSDType => x match {
            case p: XSNMToken => ObjectElement(getDefaultPrefix(), action, None, None, None, None, None, None) // that will be pattern but not supported right now in ShExML
            case _ => ObjectElement("", action, None, None, None, Some(DataTypeLiteral(x.name)), None, None)
          }
        }
      }
      case None => element.theType match {
        case Some(typeName) => ShapeLink(ShapeVar(getDefaultPrefix() + normalizeName(typeName)))
      }
    }
    val restrictions = element match {
      case t: Typeable => generateRestrictions(t).map(r =>
                if(r.equals("{1}")) "" else " " + r) //implicit boundary

      case _ => Some("")
    }

    PredicateObject(predicate, objectOrShapeLink)
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

  private def getDefaultPrefix(): String = {
    if(schema.attributes.attributes.exists(_._1 == "targetNamespace"))
      "tn:"
    else ":"
  }

}
