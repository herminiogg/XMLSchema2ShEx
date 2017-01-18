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
    val xml = """<?xml version="1.0"?>
                |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                |<xs:attribute name="id" type="xs:positiveInteger"/>
                |<xs:complexType name="station">
                |    <xs:sequence>
                |        <xs:element name="name" type="xs:string" />
                |    </xs:sequence>
                |    <xs:attribute ref="id" use="required"/>
                |</xs:complexType>
                |<xs:complexType name="driver">
                |    <xs:sequence>
                |        <xs:element name="name" type="xs:string" />
                |        <xs:element name="birth_date" type="xs:date" />
                |        <xs:element name="email" type="xs:string" />
                |        <xs:element name="phone" type="xs:string" />
                |        <xs:element name="avatar" type="xs:string" />
                |    </xs:sequence>
                |    <xs:attribute ref="id" use="required"/>
                |</xs:complexType>
                |<xs:element name="lvb_system">
                |    <xs:complexType>
                |        <xs:sequence>
                |            <xs:element name="line" minOccurs="0" maxOccurs="unbounded" >
                |                <xs:complexType>
                |                    <xs:sequence>
                |                        <xs:element name="code" type="xs:string" />
                |                        <xs:element name="type" type="xs:string" />
                |                        <xs:element name="start_time_operation" type="xs:time" />
                |                        <xs:element name="end_time_operation" type="xs:time" />
                |                        <xs:element name="count_vehicles">
                |                          <xs:simpleType>
                |                            <xs:restriction base="xs:integer">
                |                              <xs:minInclusive value="0"/>
                |                              <xs:maxInclusive value="10"/>
                |                            </xs:restriction>
                |                          </xs:simpleType>
                |                        </xs:element>
                |                        <xs:element name="map" type="xs:string" />
                |                        <xs:element name="start_station" type="station" minOccurs="0" maxOccurs="1" />
                |                        <xs:element name="end_station" type="station" minOccurs="0" maxOccurs="1" />
                |                        <xs:element name="intermediate_stations" type="station" minOccurs="0" maxOccurs="5" />
                |                        <xs:element name="vehicles_line" minOccurs="0" maxOccurs="10" >
                |                            <xs:complexType>
                |                                <xs:sequence>
                |                                    <xs:element name="name" type="xs:string" />
                |                                    <xs:element name="capacity" type="xs:integer" />
                |                                    <xs:element name="driver" minOccurs="0" maxOccurs="unbounded" type='driver' />
                |                                </xs:sequence>
                |                                <xs:attribute ref="id" use="required"/>
                |                            </xs:complexType>
                |                        </xs:element>
                |                    </xs:sequence>
                |                    <xs:attribute ref="id" use="required"/>
                |                </xs:complexType>
                |            </xs:element>
                |        </xs:sequence>
                |    </xs:complexType>
                |</xs:element>
                |</xs:schema>""".stripMargin

    val output = XMLSchema2ShexParser().parse(xml, None)
    val fw = new FileWriter("result.shex")
    fw.write(output)
    fw.close()
    println(output)
  }

}