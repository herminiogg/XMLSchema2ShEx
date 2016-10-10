/**
  * Created by herminio on 4/10/16.
  */
sealed trait Tag
sealed trait Type extends Tag
case class Element(attributes: Attributes, aType: Option[Type]) extends Tag {
  val name = attributes.attributes.get("name")
  val theType = attributes.attributes.get("theType")
  val minOccurs = attributes.attributes.get("minOccurs")
  val maxOccurs = attributes.attributes.get("maxOccurs")
}
case class ComplexType(attributes: Attributes, sequence: Sequence, attributesElements: List[AttributeElement]) extends Type {
  val name = attributes.attributes.get("name")
}
case class SimpleType(attributes: Attributes, restriction: Option[Restriction]) extends Type {
  val name = attributes.attributes.get("name")
}

sealed trait XSDType extends Type {
  def name: String
}
case class XSDInteger(name: String = "xs:integer") extends XSDType
case class XSDDecimal(name: String = "xs:decimal") extends XSDType
case class XSDString(name: String = "xs:string") extends XSDType
case class XSDDate(name: String = "xs:date") extends XSDType