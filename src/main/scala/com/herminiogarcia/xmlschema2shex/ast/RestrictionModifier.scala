package com.herminiogarcia.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
sealed trait RestrictionModifier
sealed trait BoundaryRestrictionModifier extends RestrictionModifier {
  val attributes: Attributes
  def value = attributes.attributes.get("value").map(_.replace("\"", "").toInt)
}
case class MaxExclusive(attributes: Attributes) extends BoundaryRestrictionModifier
case class MinExclusive(attributes: Attributes) extends BoundaryRestrictionModifier
case class MaxInclusive(attributes: Attributes) extends BoundaryRestrictionModifier
case class MinInclusive(attributes: Attributes) extends BoundaryRestrictionModifier
case class Pattern(attributes: Attributes) extends RestrictionModifier {
  def value = attributes.attributes.get("value").map(_.replace("\\", "\\\\"))
}
case class Enumeration(attributes: Attributes) extends RestrictionModifier