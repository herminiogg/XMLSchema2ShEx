package com.herminiogarcia.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
sealed trait AST
case class Schema(attributes: Attributes, tags: List[Tag]) extends AST