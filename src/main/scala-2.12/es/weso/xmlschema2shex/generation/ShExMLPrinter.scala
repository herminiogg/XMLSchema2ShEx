package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{Declaration, Field, Iterator, NestedIterator, ObjectElement, PredicateObject, ShExML, Shape, ShapeLink}

class ShExMLPrinter {

  def print(s: ShExML): String = {
    val declarationsToPrint = s.declaration.map {
      case Declaration(declarationStatement) => declarationStatement match {
        case i: Iterator => print(i, -1)
      }
    }
    val shapesToPrint = s.shape.map {
      case s: Shape => print(s, -1)
    }
    declarationsToPrint.mkString("") + shapesToPrint.mkString("")
  }

  def print(i: Iterator, indentation: Int): String = {
    val indentationString = generateIndentation(indentation)
    indentationString +
    "ITERATOR " + i.name.name + " <" + i.queryClause.query + "> {\n" +
      i.fields.map(print(_, indentation + 1)).mkString("") +
      i.iterators.map(print(_, indentation + 1)).mkString("") +
     indentationString + "}\n"
  }

  def print(ni: NestedIterator, indentation: Int): String = {
    val indentationString = generateIndentation(indentation)
    indentationString +
    "ITERATOR " + ni.name.name + " <" + ni.queryClause.query + "> {\n" +
      ni.fields.map(print(_, indentation + 1)).mkString("") +
      ni.iterators.map(print(_, indentation + 1)).mkString("") +
    indentationString +"}\n"
  }

  def print(f: Field, indentation: Int): String = {
    generateIndentation(indentation) +
    "FIELD " + f.name.name + " <" + f.queryClause.query + "> \n"
  }

  def print(s: Shape, indentation: Int): String = {
    generateIndentation(indentation) +
    s.shapeName.name + " " + s.shapePrefix + "[] {\n" +
      s.predicateObjects.map(po => print(po, indentation + 1)).mkString("") +
    "}\n"
  }

  def print(po: PredicateObject, indentation: Int): String = {
    val objectPart = po.objectOrShapeLink match {
      case ObjectElement(prefix, action, literalValue, matcher, dataType, langTag) => literalValue match {
        case Some(literal) => prefix + literal + dataType.getOrElse("")
        case None => prefix + "[] " + dataType.getOrElse("")
      }
      case ShapeLink(shape) => "@" + shape.name
    }
    generateIndentation(indentation) +
    po.predicate.prefix + po.predicate.`extension` + " " + objectPart + " ;\n"
  }

  private def generateIndentation(indentation: Int): String = {
    val identationStrings = (0 to indentation).map(_ => "\t")
    identationStrings.mkString("")
  }

}
