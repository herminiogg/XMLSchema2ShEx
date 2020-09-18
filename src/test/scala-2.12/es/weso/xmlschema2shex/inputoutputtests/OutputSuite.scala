package es.weso.xmlschema2shex.inputoutputtests

import es.weso.xmlschema2shex.parser.XMLSchema2ShexParser
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
  * Created by herminio on 19/12/16.
  */
class OutputSuite extends AnyFunSuite with Matchers {

  test("XML Schema conversion from Microsoft example") {
    val xml = """
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://tempuri.org/po.xsd"
               xmlns="http://tempuri.org/po.xsd" elementFormDefault="qualified">

      <xs:element name="purchaseOrder" type="PurchaseOrderType"/>

      <xs:element name="comment" type="xs:string"/>

      <xs:complexType name="PurchaseOrderType">
        <xs:sequence>
          <xs:element name="shipTo" type="USAddress"/>
          <xs:element name="billTo" type="USAddress"/>
          <xs:element ref="comment" minOccurs="0"/>
          <xs:element name="items"  type="Items"/>
        </xs:sequence>
        <xs:attribute name="orderDate" type="xs:date"/>
      </xs:complexType>

      <xs:complexType name="USAddress">

        <xs:sequence>
          <xs:element name="name"   type="xs:string"/>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city"   type="xs:string"/>
          <xs:element name="state"  type="xs:string"/>
          <xs:element name="zip"    type="xs:decimal"/>
        </xs:sequence>
        <xs:attribute name="country" type="xs:NMTOKEN"
                      fixed="US"/>
      </xs:complexType>

      <xs:complexType name="Items">
        <xs:sequence>
          <xs:element name="item" minOccurs="0" maxOccurs="unbounded">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="productName" type="xs:string"/>
                <xs:element name="quantity">
                  <xs:simpleType>
                    <xs:restriction base="xs:positiveInteger">
                      <xs:maxExclusive value="100"/>
                    </xs:restriction>
                  </xs:simpleType>
                </xs:element>
                <xs:element name="USPrice"    type="xs:decimal"/>
                <xs:element ref="comment"   minOccurs="0"/>
                <xs:element name="shipDate" type="xs:date" minOccurs="0"/>
              </xs:sequence>
              <xs:attribute name="partNum" type="SKU" use="required"/>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>

      <xs:simpleType name="SKU">
        <xs:restriction base="xs:string">
          <xs:pattern value="\d{3}-[A-Z]{2}"/>
        </xs:restriction>
      </xs:simpleType>

    </xs:schema>
    """
    val output = XMLSchema2ShexParser().parse(xml, None).stripMargin.replaceAll(" |\n", "")
    output should include ("""<PurchaseOrderType> {
                             |:shipTo @<USAddress> {1}  ;
                             |:billTo @<USAddress> {1}  ;
                             |:comment xs:string ?  ;
                             |:items @<Items> {1}  ;
                             |:orderDate xs:date {1}  ;
                             |}""".stripMargin.replaceAll(" |\n", ""))
    output should include ("""<USAddress> {
                             |:name xs:string {1}  ;
                             |:street xs:string {1}  ;
                             |:city xs:string {1}  ;
                             |:state xs:string {1}  ;
                             |:zip xs:decimal {1}  ;
                             |:country [US]  {1}  ;
                             |}""".stripMargin.replaceAll(" |\n", ""))
    output should include ("""<Items> {
                             |:item @<item> *  ;
                             |}""".stripMargin.replaceAll(" |\n", ""))
    output should include ("""<item> {
                             |:productName xs:string {1}  ;
                             |:quantity xs:positiveInteger {1, 99}  ;
                             |:USPrice xs:decimal {1}  ;
                             |:comment xs:string ?  ;
                             |:shipDate xs:date ?  ;
                             |:partNum xs:string {1} PATTERN \\d{3}-[A-Z]{2} ;
                             |}""".stripMargin.replaceAll(" |\n", ""))
  }

  test("XML Schema conversion for addresses") {
    val xml =
      """<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
        |
        | <xs:element name="addresses">
        | <xs:complexType>
        | <xs:sequence>
        | <xs:element ref="address" minOccurs='1' maxOccurs='unbounded'/>
        | </xs:sequence>
        | </xs:complexType>
        |</xs:element>
        |
        | <xs:element name="address">
        | <xs:complexType>
        | <xs:sequence>
        | <xs:element ref="name" minOccurs='0' maxOccurs='1'/>
        | <xs:element ref="street" minOccurs='0' maxOccurs='1'/>
        | </xs:sequence>
        | </xs:complexType>
        | </xs:element>
        |
        | <xs:element name="name" type='xs:string'/>
        | <xs:element name="street" type='xs:string'/>
        |</xs:schema>
      """.stripMargin
    val output = XMLSchema2ShexParser().parse(xml, None).stripMargin.replaceAll(" |\n", "")
    output should include("""<addresses> {
                            |:address @<address> + ;
                            |}""".stripMargin.replaceAll(" |\n", ""))

    output should include("""<address> {
                            |:name xs:string ? ;
                            |:street xs:string ? ;
                            |}""".stripMargin.replaceAll(" |\n", ""))

  }

  test("""Xml1 version""") {
    val xml = """<?xml version="1.0" encoding="UTF-8" ?>
                 |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                 |
                 |<xs:element name="orderperson" type="xs:string"/>
                 |<xs:element name="name" type="xs:string"/>
                 |<xs:element name="address" type="xs:string"/>
                 |<xs:element name="city" type="xs:string"/>
                 |<xs:element name="country" type="xs:string"/>
                 |<xs:element name="title" type="xs:string"/>
                 |<xs:element name="note" type="xs:string"/>
                 |<xs:element name="quantity" type="xs:positiveInteger"/>
                 |<xs:element name="price" type="xs:decimal"/>
                 |
                 |<!-- definition of attributes -->
                 |<xs:attribute name="orderid" type="xs:string"/>
                 |
                 |<!-- definition of complex elements -->
                 |<xs:element name="shipto">
                 |  <xs:complexType>
                 |    <xs:sequence>
                 |      <xs:element ref="name"/>
                 |      <xs:element ref="address"/>
                 |      <xs:element ref="city"/>
                 |      <xs:element ref="country"/>
                 |    </xs:sequence>
                 |  </xs:complexType>
                 |</xs:element>
                 |
                 |<xs:element name="item">
                 |  <xs:complexType>
                 |    <xs:sequence>
                 |      <xs:element ref="title"/>
                 |      <xs:element ref="note" minOccurs="0"/>
                 |      <xs:element ref="quantity"/>
                 |      <xs:element ref="price"/>
                 |    </xs:sequence>
                 |  </xs:complexType>
                 |</xs:element>
                 |
                 |<xs:element name="shiporder">
                 |  <xs:complexType>
                 |    <xs:sequence>
                 |      <xs:element ref="orderperson"/>
                 |      <xs:element ref="shipto"/>
                 |      <xs:element ref="item" maxOccurs="unbounded"/>
                 |    </xs:sequence>
                 |    <xs:attribute ref="orderid" use="required"/>
                 |  </xs:complexType>
                 |</xs:element>
                 |
                 |</xs:schema>""".stripMargin
    val output = XMLSchema2ShexParser().parse(xml, None).stripMargin.replaceAll(" |\n", "")
    output should include("""<shiporder> {
                            |:orderperson xs:string {1}  ;
                            |:shipto @<shipto> {1}  ;
                            |:item @<item> +  ;
                            |:orderid xs:string {1}  ;
                            |}""".stripMargin.replaceAll(" |\n", ""))
    output should include("""<item> {
                            |:title xs:string {1}  ;
                            |:note xs:string ?  ;
                            |:quantity xs:positiveInteger {1}  ;
                            |:price xs:decimal {1}  ;
                            |}""".stripMargin.replaceAll(" |\n", ""))
    output should include("""<shipto> {
                           |:name xs:string {1}  ;
                           |:address xs:string {1}  ;
                           |:city xs:string {1}  ;
                           |:country xs:string {1}  ;
                           |}""".stripMargin.replaceAll(" |\n", ""))
  }

  test("""Xml2 version""") {
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
    val output = XMLSchema2ShexParser().parse(xml, None).stripMargin.replaceAll(" |\n", "")
    output should include("""<shipordertype> {
                            |:orderperson xs:string {1}  ;
                            |:shipto @<shiptotype> {1}  ;
                            |:item @<itemtype> +  ;
                            |:orderid xs:string {1} PATTERN [0-9]{6} ;
                            |}""".stripMargin.replaceAll(" |\n", ""))
    output should include("""<itemtype> {
                            |:title xs:string {1}  ;
                            |:note xs:string ?  ;
                            |:quantity xs:positiveInteger {1}  ;
                            |:price xs:decimal {1}  ;
                            |}""".stripMargin.replaceAll(" |\n", ""))
    output should include("""<shiptotype> {
                            |:name xs:string {1}  ;
                            |:address xs:string {1}  ;
                            |:city xs:string {1}  ;
                            |:country xs:string {1}  ;
                            |}""".stripMargin.replaceAll(" |\n", ""))
  }


}
