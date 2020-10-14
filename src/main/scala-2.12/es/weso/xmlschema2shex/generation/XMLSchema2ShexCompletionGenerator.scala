package es.weso.xmlschema2shex.generation

import es.weso.shexml.ast.{AutoIncrement, Declaration, Prefix, Shape, URL, Var}

class XMLSchema2ShexCompletionGenerator(declarations: List[Declaration], shapes: List[Shape]) {

  def generate(): List[Declaration] = {
    val declarationsWithAutoIncrement = completeAutoincrement() match {
      case Some(declaration) => declarations :+ declaration
      case None => declarations
    }
    completeExamplePrefix() +: declarationsWithAutoIncrement
  }

  def completeExamplePrefix(): Declaration = {
    Declaration(
      Prefix(Var(":"), URL("http://example.org"))
    )
  }

  def completeAutoincrement(): Option[Declaration] = {
    val needForAutoincrementDeclaration = shapes.exists(s => s.action match {
      case Var(name) => name == "subjectAutoincrementId"
      case _ => false
    })
    if(needForAutoincrementDeclaration) {
      Some(Declaration(
        AutoIncrement(Var("subjectAutoincrementId"), 1, Int.MaxValue, 1, Some("subject_"), None)))
    } else None
  }

}
