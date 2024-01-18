package com.herminiogarcia.xmlschema2shex.generation

import com.herminiogarcia.shexml.ast.{Action, AutoIncrement, Declaration, Prefix, Shape, URL, Var}

class XMLSchema2ShexCompletionGenerator(declarations: List[Declaration], shapes: List[Shape]) {

  def generate(): List[Declaration] = {
    val declarationsWithAutoIncrement = completeAutoincrement() match {
      case Some(declaration) => declarations :+ declaration
      case None => declarations
    }
    completePrefixes() ::: declarationsWithAutoIncrement
  }

  def completePrefixes(): List[Declaration] = {
    List(
      Declaration(Prefix(Var(":"), URL("http://example.org"))),
      Declaration(Prefix(Var("xs:"), URL("http://www.w3.org/2001/XMLSchema#"))),
      Declaration(Prefix(Var("xsd:"), URL("http://www.w3.org/2001/XMLSchema#")))
    )

  }

  def completeAutoincrement(): Option[Declaration] = {
    val needForAutoincrementDeclaration = shapes.exists(s => s.action match {
      case Action(_, action, _) => action match {
        case Var(name) => name == "subjectAutoincrementId"
        case _ => false
      }
      case _ => false
    })
    if(needForAutoincrementDeclaration) {
      Some(Declaration(
        AutoIncrement(Var("subjectAutoincrementId"), 1, Int.MaxValue, 1, Some("subject_"), None)))
    } else None
  }

}
