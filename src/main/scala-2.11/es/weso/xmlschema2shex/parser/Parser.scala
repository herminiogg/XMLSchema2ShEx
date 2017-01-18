package es.weso.xmlschema2shex.parser

import java.io.FileWriter

import es.weso.xmlschema2shex.check.SemanticChecker
import es.weso.xmlschema2shex.decorator.{NameDecorator, TypeDecorator}
import es.weso.xmlschema2shex.generation.CodeGenerator

/**
  * Created by herminio on 4/10/16.
  */
object Parser extends XMLSchemaParser{
  def main(args: Array[String]): Unit = {
    val xml = """<?xml version="1.0" encoding="UTF-8" ?>
                |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                |
                |<xs:simpleType name="stringtype">
                |  <xs:restriction base="xs:string"/>
                |</xs:simpleType>
                |
                |<xs:simpleType name="inttype">
                |  <xs:restriction base="xs:positiveInteger"/>
                |</xs:simpleType>
                |
                |<xs:simpleType name="dectype">
                |  <xs:restriction base="xs:decimal"/>
                |</xs:simpleType>
                |
                |<xs:simpleType name="orderidtype">
                |  <xs:restriction base="xs:string">
                |    <xs:pattern value="[0-9]{6}"/>
                |  </xs:restriction>
                |</xs:simpleType>
                |
                |<xs:complexType name="shiptotype">
                |  <xs:sequence>
                |    <xs:element name="name" type="stringtype"/>
                |    <xs:element name="address" type="stringtype"/>
                |    <xs:element name="city" type="stringtype"/>
                |    <xs:element name="country" type="stringtype"/>
                |  </xs:sequence>
                |</xs:complexType>
                |
                |<xs:complexType name="itemtype">
                |  <xs:sequence>
                |    <xs:element name="title" type="stringtype"/>
                |    <xs:element name="note" type="stringtype" minOccurs="0"/>
                |    <xs:element name="quantity" type="inttype"/>
                |    <xs:element name="price" type="dectype"/>
                |  </xs:sequence>
                |</xs:complexType>
                |
                |<xs:complexType name="shipordertype">
                |  <xs:sequence>
                |    <xs:element name="orderperson" type="stringtype"/>
                |    <xs:element name="shipto" type="shiptotype"/>
                |    <xs:element name="item" maxOccurs="unbounded" type="itemtype"/>
                |  </xs:sequence>
                |  <xs:attribute name="orderid" type="orderidtype" use="required"/>
                |</xs:complexType>
                |
                |<xs:element name="shiporder" type="shipordertype"/>
                |
                |</xs:schema>""".stripMargin

    val output = XMLSchema2ShexParser().parse(xml, None)
    val fw = new FileWriter("result.shex")
    fw.write(output)
    fw.close()
    println(output)
  }

}