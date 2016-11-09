package es.weso.xmlschema2shex.check

import es.weso.xmlschema2shex.ast._

/**
  * Created by herminio on 5/10/16.
  */
class SemanticChecker(val schema: Schema) {

  def check(): Unit = {
    schema match {
      case Schema(attributes, tags) => {
        //Check attributes
        checkTags(tags)
      }
    }
  }

  def checkTags(tags: List[Tag]) = {
    for(tag <- tags) {
      tag match {
        case Element(attributes, aType) => {
          aType match {
            case Some(theType) => checkType(theType)
            case None =>
              if (!attributes.attributes.isDefinedAt("type") && !attributes.attributes.isDefinedAt("ref"))
                throw new Exception(SemanticErrors.NOTYPEDEFINED + attributes.attributes.get("name"))
          }
        }
        case c: ComplexType => checkType(c)
        case s: SimpleType => checkType(s)
      }
    }
  }

  def checkType(aType: Type): Unit =  aType match {
    case ComplexType(attributes, sequence, attributesElements) => checkTags(sequence.elements)
    case SimpleType(attributes, restriction) => restriction match {
      case Some(restriction) => checkRestriction(restriction)
    }
  }

  def checkRestriction(restriction: Restriction): Unit = {
    restriction.restrictions.foreach(println(_))
  }

}

object SemanticErrors {
  val NOTYPEDEFINED = "No type defined on the element "
}
