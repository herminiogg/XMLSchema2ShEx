package es.weso.xmlschema2shex.decorator

import es.weso.xmlschema2shex.ast._

/**
  * Created by herminio on 6/10/16.
  */
class TypeDecorator(schema: Schema) {

  def decorate(): Schema = {
    schema.tags.foldLeft(schema)((oldSchema, tag) => {
      tag match {
        case e: Element =>
          val index = oldSchema.tags.indexOf(e)
          val newElement = decorateAllElements(oldSchema.tags(index).asInstanceOf[Element])
          val newTags = oldSchema.tags.updated(index, newElement)
          oldSchema.copy(tags = newTags)
        case c: ComplexType =>
          val index = oldSchema.tags.indexOf(c)
          val newComplexType = decorateComplexType(c)
          val newTags = oldSchema.tags.updated(index, newComplexType)
          oldSchema.copy(tags = newTags)
        case _ => oldSchema
      }
    })
  }

  private def decorateComplexType(complexType: ComplexType): ComplexType = {
    val newElementSequence = complexType.sequence.elements.map(decorateAllElements)
    val newAttributesSequence = complexType.attributesElements.map(decorateAllAttributes)
    val newSequence = complexType.sequence.copy(elements = newElementSequence)
    complexType.copy(sequence = newSequence, attributesElements = newAttributesSequence)
  }

  private def decorateAllElements(element: Element): Element = {
    val newElement = element.copy(aType = searchTypeForElement(element, schema.tags))
    val newType = newElement.aType.map({
      case c: ComplexType =>
        val innerElements = for (elem <- c.sequence.elements) yield decorateAllElements(elem)
        val newAttributesSequence = c.attributesElements.map(decorateAllAttributes)
        val newSequence = c.sequence.copy(elements = innerElements)
        c.copy(sequence = newSequence, attributesElements = newAttributesSequence)
      case s: SimpleType => s
      case x: XSDType => x
    })
    newElement.copy(aType = newType)
  }

  private def decorateAllAttributes(attribute: AttributeElement): AttributeElement = {
    attribute.copy(aType = searchTypeForElement(attribute, schema.tags))
  }

  private def searchTypeForElement(element: Typeable, tags: List[Tag]): Option[Type] = {
    element.aType match {
      case Some(theType) => Some(theType)
      case None =>
        val typeName = getType(element)
        if(decorateXSDType(typeName, element.attributes).isDefined) return decorateXSDType(typeName, element.attributes)
        tags.foldLeft[Option[Type]](None)((result, tag) => {
          if(result.isDefined)
            result
          else {
            tag match {
              case c: ComplexType =>
                if(c.name.equals(typeName)) Some(c) else searchTypeForElement(element, c.sequence.elements)
              case s: SimpleType =>
                if(s.name.equals(typeName)) Some(s) else result
              case e: Element => {
                if (e.name.equals(typeName)) e.aType match {
                  case Some(theType) => Some(theType)
                  case None => {
                    val findedType = searchTypeForElement(e, tags)
                    findedType
                  }
                } else result
              }
              case _ => result
            }
          }
        })
    }
  }

  private def getType(element: Typeable): Option[String] = {
    element.attributes.attributes.get("type") match {
      case Some(theType) => Some(theType)
      case None => element.attributes.attributes.get("ref") match {
        case Some(ref) => Some(ref)
        case None => None
      }
    }
  }
  

  private def decorateXSDType(typeName: Option[String], attributes: Attributes): Option[Type] = {
    typeName match {
      case Some(s) => s.replaceAll("\"", "") match {
        case "xs:string" => Some(XSDString())
        case "xs:integer" => Some(XSDInteger())
        case "xs:decimal" => Some(XSDDecimal())
        case "xs:date" => Some(XSDDate())
        case "xs:NMTOKEN" => Some(XSNMToken(value = attributes.attributes.getOrElse("fixed", "")))
        case _ => None
      }
      case _ => None
    }
  }

}
