package es.weso.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
case class Restriction(attributes: Attributes, restrictions: Option[List[RestrictionModifier]],
                       sequence: Option[Sequence], attributesElements: Option[List[AttributeElement]]) {
  val base = attributes.attributes.get("base")
}
