/**
  * Created by herminio on 4/10/16.
  */
sealed trait RestrictionModifier
case class MaxExclusive(attributes: Attributes) extends RestrictionModifier
case class MinExclusive(attributes: Attributes) extends RestrictionModifier
case class MaxInclusive(attributes: Attributes) extends RestrictionModifier
case class MinInclusive(attributes: Attributes) extends RestrictionModifier
case class Pattern(attributes: Attributes) extends RestrictionModifier
case class Enumeration(attributes: Attributes) extends RestrictionModifier