/**
  * Created by herminio on 4/10/16.
  */
import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

class XMLSchemaParser extends JavaTokenParsers {

  def root: Parser[Schema] = "<xs:schema"~attributes~">"~rep(tags)~"</xs:schema>" ^^ {
    case _ ~ attributes ~ _ ~ tags ~ _ => Schema(attributes, tags)
  }

  def tags: Parser[Tag] = element | complexType | simpleType

  def element: Parser[Element] =
    "<xs:element"~attributes~">"~opt(simpleType)~"</xs:element>" ^^ {
      case _ ~ attributes ~ _ ~ option ~ _ => Element(attributes, option)
    } | "<xs:element"~attributes~">"~opt(complexType)~"</xs:element>" ^^ {
      case _ ~ attributes ~ _ ~ option ~ _ => Element(attributes, option)
    } |"<xs:element"~attributes~"/>" ^^ {
      case _ ~ attributes ~ _ => Element(attributes, None)
    }

  def complexType: Parser[ComplexType] = "<xs:complexType"~attributes~">"~sequence~opt(attributesList)~"</xs:complexType>" ^^ {
    case _ ~ attributes ~ _ ~ sequence ~ attributesList ~_ =>
      ComplexType(attributes, sequence, attributesList.getOrElse(List()))
  }

  def sequence: Parser[Sequence] = "<xs:sequence>"~rep1(element)~"</xs:sequence>" ^^ {
    case _ ~ elements ~ _ => Sequence(elements)
  }

  def complexContent: Parser[Any] = "<xs:complexContent>"~opt(extension)~opt(restriction)~"</xs:complexContent>"

  def extension: Parser[Any] = "<xs:extension"~attributes~">"~opt(attributesList)~"</xs:extension>"

  def attributesList: Parser[List[AttributeElement]] = rep("<xs:attribute"~attributes~"/>") ^^ {
    _.map {
      case _ ~ attributes ~ _ => AttributeElement(attributes)
    }
  }

  def simpleType: Parser[SimpleType] = "<xs:simpleType"~attributes~">"~opt(restriction)~"</xs:simpleType>" ^^ {
    case _ ~ attributes ~ _ ~ restriction ~ _ => SimpleType(attributes, restriction)
  }

  def attributes: Parser[Attributes] = rep("[A-Za-z:]*=\"[A-Za-z0-9:/.#{}\\-\\[\\]\\\\]*\"".r) ^^ { a => {
    Attributes(a.map(_.split("=").toList match {
      case x :: xs => (x.toString, xs.head.toString)
    }).toMap)
  }
  }

  def restriction: Parser[Restriction] = "<xs:restriction"~attributes~">"~rep(restrictions) ~"</xs:restriction>" ^^ {
    case _ ~ attributes ~ _ ~ restrictions ~ _ => Restriction(attributes, Some(restrictions), None, None)
  } |
    "<xs:restriction"~attributes~">"~sequence~opt(attributesList) ~"</xs:restriction>" ^^ {
      case _ ~ attributes ~_ ~ sequence ~ attributeList ~ _ => Restriction(attributes, None, Some(sequence), attributeList)
  }

  def restrictions: Parser[RestrictionModifier] = maxExclusiveRestriction | minExclusiveRestriction |
    maxInclusiveRestriction | minInclusiveRestriction | patternRestriction | enumeration

  def maxExclusiveRestriction: Parser[MaxExclusive] = "<xs:maxExclusive"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MaxExclusive(attributes)
  }

  def minExclusiveRestriction: Parser[MinExclusive] = "<xs:minExclusive"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MinExclusive(attributes)
  }

  def maxInclusiveRestriction: Parser[MaxInclusive] = "<xs:maxInclusive"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MaxInclusive(attributes)
  }

  def minInclusiveRestriction: Parser[MinInclusive] = "<xs:minInclusive"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MinInclusive(attributes)
  }

  def patternRestriction: Parser[Pattern] = "<xs:pattern"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => Pattern(attributes)
  }

  def enumeration: Parser[Enumeration] = "<xs:enumeration"~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => Enumeration(attributes)
  }

  def annotation: Parser[Any] = "<xs:annotation>"~"".r~"</xs:annotation>"

}
