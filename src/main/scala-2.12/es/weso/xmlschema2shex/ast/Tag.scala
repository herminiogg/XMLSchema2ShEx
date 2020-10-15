package es.weso.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
sealed trait Tag {}
sealed trait Type extends Tag
sealed trait Typeable {
  val aType: Option[Type]
  val attributes: Attributes
  val name: Option[String]
  val ref: Option[String]
  val theType = attributes.attributes.get("type")

  def minOccurs = {
    val minOccursAttributes = attributes.attributes.get("minOccurs").map(_.replaceAll("\"", ""))
    minOccursAttributes match {
      case Some(minOccurs) => Some(minOccurs)
      case _ => aType.getOrElse(None) match {
        case s: SimpleType => s.restriction.get.restrictions.getOrElse(Nil).
          find(r => r.isInstanceOf[MinExclusive] || r.isInstanceOf[MinInclusive]).map({
          case me: MinExclusive => me.value.map(_ + 1).map(_.toString).get
          case mi: MinInclusive => mi.value.map(_.toString).get
        })
        case _ => None
      }
    }
  }

  def maxOccurs = {
    val maxOccursAttributes = attributes.attributes.get("maxOccurs").map(_.replace("\"", ""))
    maxOccursAttributes match {
      case Some(maxOccurs) => Some(maxOccurs)
      case _ => aType.getOrElse(None) match {
        case s: SimpleType => s.restriction.get.restrictions.getOrElse(Nil).
          find(r => r.isInstanceOf[MaxExclusive] || r.isInstanceOf[MaxInclusive]).map({
          case me: MaxExclusive => me.value.map(_ - 1).map(_.toString).get
          case mi: MaxInclusive => mi.value.map(_.toString).get
        })
        case _ => None
      }
    }
  }

  def pattern = {
    aType.getOrElse(None) match {
      case s: SimpleType if s.restriction.get.base.getOrElse("").replace("\"", "").equals("xs:string") =>
        s.restriction.get.restrictions.getOrElse(Nil).find(_.isInstanceOf[Pattern]).flatMap(_.asInstanceOf[Pattern].value)
      case _ => None
    }
  }
}

case class Element(attributes: Attributes, aType: Option[Type]) extends Tag with Typeable {
  val name = attributes.attributes.get("name")
  val ref = attributes.attributes.get("ref")

}

case class AttributeElement(attributes: Attributes, aType: Option[Type]) extends Tag with Typeable {
  val name: Option[String] = attributes.attributes.get("name")
  val ref: Option[String] = attributes.attributes.get("ref")
}

case class ComplexType(attributes: Attributes, elementsHolder: ElementsHolder, attributesElements: List[AttributeElement]) extends Type {
  val name = attributes.attributes.get("name")
  val ref = attributes.attributes.get("ref")
}

case class SimpleType(attributes: Attributes, restriction: Option[Restriction]) extends Type {
  def name = {
    val attributeName = attributes.attributes.get("name")
    if(attributeName.isDefined) attributeName
    else {
      restriction.get.base
    }
  }
}

sealed trait XSDType extends Type {
  def name: String
}
case class XSDInteger(name: String = "xs:integer") extends XSDType
case class XSDDecimal(name: String = "xs:decimal") extends XSDType
case class XSDString(name: String = "xs:string") extends XSDType
case class XSDDate(name: String = "xs:date") extends XSDType
case class XSDPositiveInteger(name: String = "xs:positiveInteger") extends XSDType
case class XSDBoolean(name: String = "xs:boolean") extends XSDType
case class XSDDouble(name: String = "xs:double") extends XSDType
case class XSDDuration(name: String = "xs:duration") extends XSDType
case class XSDDateTime(name: String = "xs:dateTime") extends XSDType
case class XSDTime(name: String = "xs:time") extends XSDType
case class XSDGYearMonth(name: String = "xs:gYearMonth") extends XSDType
case class XSDGYear(name: String = "xs:gYear") extends XSDType
case class XSDGMonthDay(name: String = "xs:gMonthDay") extends XSDType
case class XSDGDay(name: String = "xs:gDay") extends XSDType
case class XSDGMonth(name: String = "xs:gMonth") extends XSDType
case class XSDHexBinary(name: String = "xs:hexBinary") extends XSDType
case class XSDBase64Binary(name: String = "xs:base64Binary") extends XSDType
case class XSDAnyURI(name: String = "xs:anyURI") extends XSDType
case class XSDQName(name: String = "xs:QName") extends XSDType
case class XSDNotation(name: String = "xs:NOTATION") extends XSDType
case class XSNMToken(name: String = "xs:NMToken", value: String) extends XSDType
case class XSDNormalizedString(name: String = "xs:normalizedString") extends XSDType
case class XSDToken(name: String = "xs:token", value: String) extends XSDType
case class XSDLanguage(name: String = "xs:language") extends XSDType
case class XSDNMTokens(name: String = "xs:NMTokens", value: String) extends XSDType
case class XSDName(name: String = "xs:name") extends XSDType
case class XSDNCName(name: String = "xs:NCName") extends XSDType
case class XSDID(name: String = "xs:ID") extends XSDType
case class XSDIDREF(name: String = "xs:IDREF") extends XSDType
case class XSDIDREFS(name: String = "xs:IDREFS") extends XSDType
case class XSDENTITY(name: String = "xs:ENTITY") extends XSDType
case class XSDNonPositiveInteger(name: String = "xs:positiveInteger") extends XSDType
case class XSDNegativeInteger(name: String = "xs:negativeInteger") extends XSDType
case class XSDLong(name: String = "xs:long") extends XSDType
case class XSDInt(name: String = "xs:int") extends XSDType
case class XSDShort(name: String = "xs:short") extends XSDType
case class XSDByte(name: String = "xs:byte") extends XSDType
case class XSDNonNegativeInteger(name: String = "xs:nonNegativeInteger") extends XSDType
case class XSDUnsignedLong(name: String = "xs:unsignedLong") extends XSDType
case class XSDUnsignedInt(name: String = "xs:unsignedInt") extends XSDType
case class XSDUnsignedShort(name: String = "xs:unsignedShort") extends XSDType
case class XSDUnsignedByte(name: String = "xs:unsignedByte") extends XSDType