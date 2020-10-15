package es.weso.xmlschema2shex.parser

/**
  * Created by herminio on 4/10/16.
  */
import es.weso.xmlschema2shex.ast._

import scala.util.parsing.combinator._

class XMLSchemaParser extends JavaTokenParsers {

  def root: Parser[Schema] = opt("<?xml"~attributes~"?>")~openingTag("schema")~attributes~">"~rep(tags)~closingTag("schema") ^^ {
    case _ ~ _ ~ attributes ~ _ ~ tags ~ _ => Schema(attributes, tags)
  }

  def tags: Parser[Tag] = element | complexType | simpleType | attribute

  def element: Parser[Element] =
    openingTag("element")~attributes~">"~opt(simpleType)~closingTag("element") ^^ {
      case _ ~ attributes ~ _ ~ option ~ _ => Element(attributes, option)
    } | openingTag("element")~attributes~">"~opt(complexType)~closingTag("element") ^^ {
      case _ ~ attributes ~ _ ~ option ~ _ => Element(attributes, option)
    } |openingTag("element")~attributes~"/>" ^^ {
      case _ ~ attributes ~ _ => Element(attributes, None)
    }

  def attribute: Parser[AttributeElement] =
    openingTag("attribute")~attributes~">"~opt(simpleType)~closingTag("attribute") ^^ {
    case _ ~ attributes ~ _ ~ option ~ _ => AttributeElement(attributes, option)
  } | openingTag("attribute")~attributes~">"~opt(complexType)~closingTag("attribute") ^^ {
    case _ ~ attributes ~ _ ~ option ~ _ => AttributeElement(attributes, option)
  } |openingTag("attribute")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => AttributeElement(attributes, None)
  }

  def complexType: Parser[ComplexType] = openingTag("complexType")~attributes~">"~(sequence | all)~opt(attributesList)~closingTag("complexType") ^^ {
    case _ ~ attributes ~ _ ~ sequence ~ attributesList ~_ =>
      ComplexType(attributes, sequence, attributesList.getOrElse(List()))
    case _ ~ attributes ~ _ ~ all ~ attributesList ~_ =>
      ComplexType(attributes, all, attributesList.getOrElse(List()))
  }

  def sequence: Parser[ElementsHolder] = fullOpeningTag("sequence")~rep1(element)~closingTag("sequence") ^^ {
    case _ ~ elements ~ _ => Sequence(elements)
  }

  def all: Parser[ElementsHolder] = fullOpeningTag("all")~rep1(element)~closingTag("all") ^^ {
    case _ ~ elements ~ _ => All(elements)
  }

  def complexContent: Parser[Any] = fullOpeningTag("complexContent")~opt(extension)~opt(restriction)~closingTag("complexContent")

  def extension: Parser[Any] = openingTag("extension")~attributes~">"~opt(attributesList)~closingTag("extension")

  def attributesList: Parser[List[AttributeElement]] = rep(openingTag("attribute")~attributes~"/>") ^^ {
    _.map {
      case _ ~ attributes ~ _ => AttributeElement(attributes, None)
    }
  }

  def simpleType: Parser[SimpleType] = openingTag("simpleType")~attributes~">"~opt(restriction)~closingTag("simpleType") ^^ {
    case _ ~ attributes ~ _ ~ restriction ~ _ => SimpleType(attributes, restriction)
  }

  def attributes: Parser[Attributes] = rep("[A-Za-z:]*=(\"|')[A-Za-z0-9:_/.#{}\\-\\[\\]\\\\]*(\"|')".r) ^^ { a => {
    Attributes(a.map(_.split("=").toList match {
      case x :: xs => (x.toString, xs.head.toString)
    }).toMap)
  }
  }

  def restriction: Parser[Restriction] = openingTag("restriction")~attributes~">"~rep(restrictions) ~closingTag("restriction") ^^ {
    case _ ~ attributes ~ _ ~ restrictions ~ _ => Restriction(attributes, Some(restrictions), None, None)
  } |
    openingTag("restriction")~attributes~">"~sequence~opt(attributesList) ~closingTag("restriction") ^^ {
      case _ ~ attributes ~_ ~ sequence ~ attributeList ~ _ => Restriction(attributes, None, Some(sequence.asInstanceOf[Sequence]), attributeList)
  } | openingTag("restriction")~attributes~"/>" ^^{
    case _ ~ attributes ~ _ => Restriction(attributes, None, None, None)
  }

  def restrictions: Parser[RestrictionModifier] = maxExclusiveRestriction | minExclusiveRestriction |
    maxInclusiveRestriction | minInclusiveRestriction | patternRestriction | enumeration

  def maxExclusiveRestriction: Parser[MaxExclusive] = openingTag("maxExclusive")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MaxExclusive(attributes)
  }

  def minExclusiveRestriction: Parser[MinExclusive] = openingTag("minExclusive")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MinExclusive(attributes)
  }

  def maxInclusiveRestriction: Parser[MaxInclusive] = openingTag("maxInclusive")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MaxInclusive(attributes)
  }

  def minInclusiveRestriction: Parser[MinInclusive] = openingTag("minInclusive")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => MinInclusive(attributes)
  }

  def patternRestriction: Parser[Pattern] = openingTag("pattern")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => Pattern(attributes)
  }

  def enumeration: Parser[Enumeration] = openingTag("enumeration")~attributes~"/>" ^^ {
    case _ ~ attributes ~ _ => Enumeration(attributes)
  }

  def annotation: Parser[Any] = openingTag("annotation")~"".r~closingTag("annotation")

  private def openingTag(name: String) = ("<(xs|xsd):" + name).r

  private def fullOpeningTag(name: String) = ("<(xs|xsd):" + name + ">").r

  private def closingTag(name: String) = ("<\\/(xs|xsd):" + name + ">").r


}
