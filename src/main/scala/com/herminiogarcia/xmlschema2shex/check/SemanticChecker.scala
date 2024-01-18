package com.herminiogarcia.xmlschema2shex.check

import com.herminiogarcia.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, Restriction, Schema, SimpleType, Tag, Type, Typeable}

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
        case e: Element => checkElement(e)
        case a: AttributeElement => checkElement(a)
        case c: ComplexType => checkType(c)
        case s: SimpleType => checkType(s)
      }
    }
  }

  def checkElement(t: Typeable) = {
    t.aType match {
      case Some(theType) => checkType(theType)
      case None =>
        if (!t.attributes.attributes.isDefinedAt("type") && !t.attributes.attributes.isDefinedAt("ref"))
          throw new Exception(SemanticErrors.NOTYPEDEFINED + t.attributes.attributes.get("name"))
    }
  }

  def checkType(aType: Type): Unit =  aType match {
    case ComplexType(attributes, sequence, attributesElements) => checkTags(sequence.elements)
    case SimpleType(attributes, restriction) => restriction match {
      case Some(restriction) => checkRestriction(restriction)
    }
  }

  def checkRestriction(restriction: Restriction): Unit = {
    //restriction.restrictions.foreach(println(_))
  }

}

object SemanticErrors {
  val NOTYPEDEFINED = "No type defined on the element "
}
