package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{AutoIncrement, Declaration, Expression, Field, Iterator, IteratorQuery, NestedIterator, ObjectElement, PredicateObject, Prefix, ShExML, Shape, ShapeLink, Source, URL, Var}

class ShExMLPrinter {

  def print(s: ShExML): String = {
    val declarationsToPrint = s.declaration.map {
      case Declaration(declarationStatement) => declarationStatement match {
        case p: Prefix => print(p)
        case i: Iterator => print(i, -1)
        case s: Source => print(s)
        case e: Expression => print(e)
        case a: AutoIncrement => print(a)
      }
    }
    val shapesToPrint = s.shape.map {
      case s: Shape => print(s, -1)
    }
    declarationsToPrint.mkString("") + shapesToPrint.mkString("")
  }

  def print(s: Source): String = {
    "SOURCE " + s.name.name + " <" + s.path.asInstanceOf[URL].url + ">\n"
  }

  def print(i: Iterator, indentation: Int): String = {
    val indentationString = generateIndentation(indentation)
    indentationString +
    "ITERATOR " + i.name.name + " <xpath: " + i.queryClause.query + "> {\n" +
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
    val shapeAction = s.action match {
      case Var(action) => action
      case _ => ""
    }
    generateIndentation(indentation) +
    s.shapeName.name + " " + s.shapePrefix + "[" + shapeAction + "] {\n" +
      s.predicateObjects.map(po => print(po, indentation + 1)).mkString("") +
    "}\n"
  }

  def print(po: PredicateObject, indentation: Int): String = {
    val objectPart = po.objectOrShapeLink match {
      case ObjectElement(prefix, action, literalValue, matcher, dataType, langTag) => literalValue match {
        case Some(literal) => prefix + literal + dataType.getOrElse("")
        case None => {
          val actionString = if(action.isDefined) action.get.asInstanceOf[Var].name else ""
          prefix + "[" + actionString + "] " + dataType.getOrElse("")
        }
      }
      case ShapeLink(shape) => "@" + shape.name
    }
    generateIndentation(indentation) +
    po.predicate.prefix + po.predicate.`extension` + " " + objectPart + " ;\n"
  }

  def print(exp: Expression): String = {
    val iteratorQuery = exp.exp.asInstanceOf[IteratorQuery]
    val secondVar = iteratorQuery.composedVar.asInstanceOf[Var]
    "EXPRESSION " + exp.name.name + " <" + iteratorQuery.firstVar.name + "." + secondVar.name + ">\n"
  }

  def print(autoId: AutoIncrement): String = {
    val precedentString = autoId.precedentString.map('"' + _ + "\"" + " + ").getOrElse("")
    val closingString = autoId.closingString.map(" + " + '"' + _ + "\"").getOrElse("")
    "AUTOINCREMENT " + autoId.name.name + " <" + precedentString + autoId.from + " to " +
      autoId.to + " by " + autoId.by + closingString + ">\n"
  }

  def print(prefix: Prefix): String = {
    "PREFIX " + prefix.name.name + " <" + prefix.url.url + ">\n"
  }

  private def generateIndentation(indentation: Int): String = {
    val indentationStrings = (0 to indentation).map(_ => "\t")
    indentationStrings.mkString("")
  }

}
