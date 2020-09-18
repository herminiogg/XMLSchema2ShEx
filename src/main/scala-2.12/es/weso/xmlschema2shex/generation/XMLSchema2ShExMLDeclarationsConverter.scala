package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{AST, Declaration, DeclarationStatement, Field, FieldQuery, Iterator, NestedIterator, ShExML, Var, XmlPath}
import es.weso.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, Schema, Sequence, SimpleType, Typeable}

class XMLSchema2ShExMLDeclarationsConverter(schema: Schema, implicit val varTable: Map[String, Typeable]) extends NameNormalizator {

  def convert(): ShExML = {
    val elements =
      for(tag <- schema.tags if tag.isInstanceOf[Element]) yield {
        convertElement(tag.asInstanceOf[Element])
    }
    val declarations = elements.map(d => Declaration(d.asInstanceOf[DeclarationStatement]))
    ShExML(declarations, Nil, Nil)
  }

  def convertElement(e: Element): AST = e.aType match {
    case Some(nestedType) => nestedType match {
      case c: ComplexType => convertComplexType(c, e)
      case s: SimpleType => convertSimpleType(s, e)
      case _ => generatePlainElement(e)
    }
    case None => generatePlainElement(e)
  }

  private def generatePlainElement(e: Element): Field = {
    val name = normalizeName(e.name, e.ref)
    Field(Var(name), FieldQuery(name))
  }

  def convertComplexType(c: ComplexType, e: Element): Iterator = {
    val attributeElementsFields = c.attributesElements.map(convertAttributeElement)
    val sequenceResults = convertSequence(c.sequence)
    val iteratorsFromSequence = sequenceResults.filter(_.isInstanceOf[Iterator]).map(_.asInstanceOf[Iterator])
    val nestedIterators = iteratorsFromSequence.map(i =>
      NestedIterator(i.name, XmlPath(i.queryClause.query.substring(1)), i.fields, i.iterators))
    val fieldsFromSequence = sequenceResults.filter(_.isInstanceOf[Field]).map(_.asInstanceOf[Field])
    val fields = attributeElementsFields ::: fieldsFromSequence
    val name = normalizeName(e.name, c.ref)
    Iterator(Var(name), XmlPath("/" + name), fields, nestedIterators)
  }

  def convertSimpleType(s: SimpleType, e: Element): Field = {
    val name = normalizeName(e.name, e.ref)
    Field(Var(name), FieldQuery(name))
  }

  def convertAttributeElement(ae: AttributeElement): Field = {
    val name = normalizeName(ae.name, ae.ref)
    Field(Var(name), FieldQuery(name))
  }

  def convertSequence(s: Sequence): List[AST] = {
    s.elements.map(convertElement)
  }
}
