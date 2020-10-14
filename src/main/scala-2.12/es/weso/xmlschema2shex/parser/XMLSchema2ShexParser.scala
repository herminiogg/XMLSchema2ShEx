package es.weso.xmlschema2shex.parser

import es.weso.shexml.ast.ShExML
import es.weso.xmlschema2shex.ast.Typeable
import es.weso.xmlschema2shex.check.SemanticChecker
import es.weso.xmlschema2shex.decorator.{NameDecorator, TypeDecorator}
import es.weso.xmlschema2shex.generation.{CodeGenerator, CodeGeneratorShExML, ShExMLPrinter, VarTableBuilder, XMLSchema2ShExMLDeclarationsConverter, XMLSchema2ShExMLShapesGeneration, XMLSchema2ShexCompletionGenerator}

import scala.collection.mutable

/**
  * Created by herminio on 5/12/16.
  */
case class XMLSchema2ShexParser() extends XMLSchemaParser {

  def parse(xmlSchema: String, context: Option[String]): String = {
    val xmlSchemaWithoutComments = removeComments(xmlSchema)
    val schema = parseAll(root, xmlSchemaWithoutComments).get
    new SemanticChecker(schema).check()
    val decoratedSchema = new TypeDecorator(new NameDecorator(schema).decorate()).decorate()
    new CodeGenerator(decoratedSchema).generate()
  }

  def convertToShExML(xmlSchema: String): String = {
    val xmlSchemaWithoutComments = removeComments(xmlSchema)
    val schema = parseAll(root, xmlSchemaWithoutComments).get
    new SemanticChecker(schema).check()
    val decoratedSchema = new TypeDecorator(new NameDecorator(schema).decorate()).decorate()
    val varTable = mutable.HashMap[String, Typeable]()
    new VarTableBuilder(varTable).visit(decoratedSchema)
    val declarations = new XMLSchema2ShExMLDeclarationsConverter(decoratedSchema, varTable.toMap).convert().declaration
    val shapes = new XMLSchema2ShExMLShapesGeneration(decoratedSchema).generate().shape
    val finalDeclarations = new XMLSchema2ShexCompletionGenerator(declarations, shapes).generate()
    new ShExMLPrinter().print(ShExML(finalDeclarations, Nil, shapes))
  }

  private def removeComments(xmlSchema: String): String = {
    xmlSchema.replaceAll("<!-- .* -->", "")
  }

}
