package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{AST, Declaration, DeclarationStatement, Expression, Field, FieldQuery, Iterator, IteratorQuery, NestedIterator, Prefix, QueryClause, ShExML, Source, URL, Var, XmlPath}
import es.weso.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, ElementsHolder, Schema, Sequence, SimpleType, Typeable}

class XMLSchema2ShExMLDeclarationsConverter(schema: Schema, implicit val varTable: Map[String, Typeable]) extends NameNormalizator {

  def convert(): ShExML = {
    val elements =
      (for(tag <- schema.tags if tag.isInstanceOf[Element] && tag.asInstanceOf[Element].aType.exists(_.isInstanceOf[ComplexType])) yield {
        convertElement(tag.asInstanceOf[Element])
    }).filter(t => t.isInstanceOf[Iterator])
    val prefix = schema.attributes.attributes.find(_._1 == "targetNamespace") match {
      case Some((_, value)) => List(Prefix(Var("tn:"), URL(value.replaceAll("\"", ""))))
      case _ => Nil
    }
    val source = List(Source(Var("example"), URL("http://example.com/example.xml")))
    val iterator = List(Expression(Var("exp"), IteratorQuery(source.head.name, elements.head.asInstanceOf[Iterator].name)))
    val declarations = (prefix ::: source ::: elements ::: iterator)
      .map(d => Declaration(d.asInstanceOf[DeclarationStatement]))
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
    Field(Var(name), FieldQuery(name), false, false)
  }

  def convertComplexType(c: ComplexType, e: Element): Iterator = {
    val attributeElementsFields = c.attributesElements.map(convertAttributeElement)
    val sequenceResults = convertSequence(c.elementsHolder)
    val iteratorsFromSequence = sequenceResults.filter(_.isInstanceOf[Iterator]).map(_.asInstanceOf[Iterator])
    val nestedIterators = iteratorsFromSequence.map(i =>
      NestedIterator(i.name, XmlPath(i.queryClause.asInstanceOf[QueryClause].query.substring(1)), i.fields, i.iterators))
    val fieldsFromSequence = sequenceResults.filter(_.isInstanceOf[Field]).map(_.asInstanceOf[Field])
    val fields = attributeElementsFields ::: fieldsFromSequence
    val nameOption = if(e.name.isDefined) e.name else e.ref
    val refOption = if(c.ref.isDefined) c.ref else c.name
    val name = normalizeName(nameOption, refOption)
    Iterator(Var(name), XmlPath("/" + name), fields, nestedIterators)
  }

  def convertSimpleType(s: SimpleType, e: Element): Field = {
    val name = normalizeName(e.name, e.ref)
    Field(Var(name), FieldQuery(name), false, false)
  }

  def convertAttributeElement(ae: AttributeElement): Field = {
    val name = normalizeName(ae.name, ae.ref)
    Field(Var(name), FieldQuery(name), false, false)
  }

  def convertSequence(e: ElementsHolder): List[AST] = {
    e.elements.map(convertElement)
  }
}
