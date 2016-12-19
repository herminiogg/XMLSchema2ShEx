package es.weso.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
sealed trait Tag
sealed trait Type extends Tag
sealed trait Typeable {
  val aType: Option[Type]
  val attributes: Attributes
  val name: Option[String]
  val ref: Option[String]

  def minOccurs = {
    val minOccursAttributes = attributes.attributes.get("minOccurs").map(_.replaceAll("\"", ""))
    minOccursAttributes match {
      case Some(minOccurs) => Some(minOccurs)
      case _ => aType.getOrElse(None) match {
        case s: SimpleType => s.restriction.get.restrictions.get.
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
        case s: SimpleType => s.restriction.get.restrictions.get.
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
        s.restriction.get.restrictions.get.find(_.isInstanceOf[Pattern]).map(_.asInstanceOf[Pattern].value).get
      case _ => None
    }
  }
}


case class Element(attributes: Attributes, aType: Option[Type]) extends Tag with Typeable {
  val name = attributes.attributes.get("name")
  val ref = attributes.attributes.get("ref")
  val theType = attributes.attributes.get("theType")

}

case class AttributeElement(attributes: Attributes, aType: Option[Type]) extends Typeable {
  val theType = attributes.attributes.get("type")
  val name: Option[String] = attributes.attributes.get("name")
  val ref: Option[String] = attributes.attributes.get("ref")
}

case class ComplexType(attributes: Attributes, sequence: Sequence, attributesElements: List[AttributeElement]) extends Type {
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
case class XSNMToken(name: String = "xs:NMToken", value: String) extends XSDType