package es.weso.xmlschema2shex.parser

import es.weso.xmlschema2shex.check.SemanticChecker
import es.weso.xmlschema2shex.decorator.{NameDecorator, TypeDecorator}
import es.weso.xmlschema2shex.generation.CodeGenerator

/**
  * Created by herminio on 5/12/16.
  */
case class XMLSchema2ShexParser() extends XMLSchemaParser {

  def parse(xmlSchema: String, context: Option[String]): String = {
    val schema = parseAll(root, xmlSchema).get
    new SemanticChecker(schema).check()
    val decoratedSchema = new TypeDecorator(new NameDecorator(schema).decorate()).decorate()
    new CodeGenerator(decoratedSchema).generate()
  }

}
