package es.weso.xmlschema2shex.decorator

import es.weso.xmlschema2shex.ast.{ComplexType, Schema, Element}

/**
  * Created by herminio on 6/10/16.
  */
class NameDecorator(schema: Schema) {

  def decorate(): Schema = {
    schema.tags.foldLeft(schema)((oldSchema, tag) => {
      tag match {
        case c: ComplexType => {
          val index = schema.tags.indexOf(tag)
          val tags = schema.tags.updated(index, decorateComplexType(c))
          schema.copy(tags = tags)
        }
        case _ => oldSchema
      }
    })
  }

  def decorateComplexType(complexType: ComplexType): ComplexType = {
    val newElements = complexType.sequence.elements.map(element => {
      val newElement = element.aType match {
        case Some(theType) => theType match {
          case c: ComplexType => {
            val newAttributes = c.attributes.attributes.updated("name", element.name.get)
            val newOuterAttributes = c.attributes.copy(attributes = newAttributes)
            val newComplexType = decorateComplexType(c).copy(attributes = newOuterAttributes)
            val newElement = element.copy(aType = Some(newComplexType))
            newElement
          }
          case _ => element
        }
        case _ => element
      }
      if(newElement.name.isEmpty) {
        val newAttributes = newElement.attributes.attributes.updated("name", newElement.ref.getOrElse(""))
        val newOuterAttributes = newElement.attributes.copy(attributes = newAttributes)
        newElement.copy(attributes = newOuterAttributes)
      } else newElement
    })
    val newSequence = complexType.sequence.copy(elements = newElements)
    complexType.copy(sequence = newSequence)
  }

}
