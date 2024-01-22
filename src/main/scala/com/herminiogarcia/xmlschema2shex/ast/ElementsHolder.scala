package com.herminiogarcia.xmlschema2shex.ast

/**
  * Created by herminio on 4/10/16.
  */
sealed trait ElementsHolder {
  def elements: List[Element]
  def copyInstance(elements: List[Element]): ElementsHolder = this match {
    case s: Sequence => s.copy(elements)
    case a: All => a.copy(elements)
  }
}
case class Sequence(elements: List[Element]) extends ElementsHolder
case class All(elements: List[Element]) extends ElementsHolder