package es.weso.xmlschema2shex.generation

import es.weso.xmlschema2shex.ast.{AttributeElement, ComplexType, Element, Schema, SimpleType, Tag, Typeable}

import scala.collection.mutable

class VarTableBuilder(varTable: mutable.Map[String, Typeable]) {

  def visit(schema: Schema): Unit = {
    schema.tags.foreach(visit)
  }

  def visit(tag: Tag): Unit = tag match {
    case e: Element => {
      registerVar(e)
      e.aType.foreach(visit)
    }
    case ae: AttributeElement => {
      registerVar(ae)
      ae.aType.foreach(visit)
    }
    case c: ComplexType => {
      c.elementsHolder.elements.foreach(visit)
      c.attributesElements.foreach(visit)
    }

    case _ =>
  }

  private def registerVar(tag: Typeable): Unit = {
    if(tag.name.isDefined) {
      varTable += ((tag.name.get, tag))
    }
  }

}
