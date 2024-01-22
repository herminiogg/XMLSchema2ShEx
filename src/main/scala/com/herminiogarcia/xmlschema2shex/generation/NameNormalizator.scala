package com.herminiogarcia.xmlschema2shex.generation

import com.herminiogarcia.xmlschema2shex.ast.Typeable

trait NameNormalizator {

  def normalizeName(name: Option[String], ref: Option[String])(implicit varTable: Map[String, Typeable]): String = {
    val output = name match {
      case Some(theName) => theName
      case None => ref match {
        case Some(theRef) => varTable.get(theRef).flatMap(_.name).getOrElse("")
        case None => throw new Exception("No reference to generate the FIELD")
      }
    }
    normalizeName(output)
  }

  def normalizeName(name: String): String = {
    name.stripPrefix("\"").stripPrefix("'").stripSuffix("'").stripSuffix("\"")
  }

}
