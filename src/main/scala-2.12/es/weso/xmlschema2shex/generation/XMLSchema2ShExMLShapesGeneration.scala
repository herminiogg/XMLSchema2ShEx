package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{ExpOrVar, ObjectElement, Predicate, PredicateObject, ShExML, Shape, ShapeLink, ShapeVar, Var}
import es.weso.xmlschema2shex.ast._

/**
  * Created by herminio on 5/10/16.
  */
class XMLSchema2ShExMLShapesGeneration(schema: Schema) extends NameNormalizator {

  val alreadyGeneratedShape = scala.collection.mutable.ListBuffer.empty[ComplexType]
  val generatedShapes = scala.collection.mutable.HashMap.empty[ComplexType, Shape]

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
    generatedShapes.values.toList.reverse
  }

  def generateComplexType(complexType: ComplexType, elementName: Option[String], precedingNavigation: String): Unit = {
    val name = complexType.name match {
      case Some(aName) => aName
      case None => elementName match {
        case Some(eName) => eName
        case None => throw new Exception("No name to generate the shape")
      }
    }
    val shapeVar = ShapeVar(":" + normalizeName(name))
    val prefix = ":" // to change
    val precedingNavigationString =
      if(precedingNavigation.isEmpty) "iterator" else precedingNavigation + "." + normalizeName(name)
    val predicateObjects = generateSequence(complexType.sequence, complexType.attributesElements, precedingNavigationString)
    val action = predicateObjects.find(po => po.predicate.`extension`.matches("[a-zA-Z0-9]*id[a-zA-Z0-9]*")
        && po.objectOrShapeLink.isInstanceOf[ObjectElement]) match {
      case Some(id) => id.objectOrShapeLink.asInstanceOf[ObjectElement].action.orNull
      case None => null
    }
    val shape = Shape(shapeVar, prefix, action, predicateObjects, None)

    for(element <- complexType.sequence.elements) yield element.aType match {
      case Some(nestedType) => nestedType match {
        case c: ComplexType if !alreadyGeneratedShape.contains(c) => generateComplexType(c, element.name, precedingNavigationString)
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

  def generateSequence(sequence: Sequence, attributes: List[AttributeElement], precedingActionNavigation: String): List[PredicateObject] = {
    val elementsPredicateObjects =
      (for(element <- sequence.elements)
      yield generateElement(element, precedingActionNavigation))
    val attributesPredicateObjects =
      (for(attribute <- attributes)
      yield generateElement(attribute, precedingActionNavigation))
    elementsPredicateObjects ::: attributesPredicateObjects
  }

  def generateElement(element: Typeable, precedingActionNavigation: String): PredicateObject = {
    val predicate = element.name match {
      case Some(theName) => Predicate(":", normalizeName(theName))
      case None => element.ref match {
        case Some(ref) => Predicate(":", normalizeName(ref))
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
            val name = c.name.getOrElse(c.ref.getOrElse(element.name.getOrElse(element.ref.get)))
            ShapeLink(ShapeVar(":" + normalizeName(name)))
          case s: SimpleType => {
            s.restriction match {
              case Some(restriction) => restriction.base match {
                case Some(name) =>
                  ObjectElement(":", action, None, None, Some(normalizeName(name)), None)
                case None => ObjectElement(":", action, None, None, s.name, None)
              }
              case None => ObjectElement(":", action, None, None, s.name, None)
            }
          }
          case x: XSDType => x match {
            case p: XSNMToken => ObjectElement(":", action, None, None, Some(p.value), None)
            case _ => ObjectElement(":", action, None, None, Some(x.name), None)
          }
        }
      }
      case None => element.theType match {
        case Some(typeName) => ShapeLink(ShapeVar(":" + normalizeName(typeName)))
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

}
