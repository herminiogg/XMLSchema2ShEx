package es.weso.xmlschema2shex.inputoutputtests

import es.weso.xmlschema2shex.parser.XMLSchema2ShexParser
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by herminio on 19/12/16.
  */
class OutputSuite extends FunSuite with Matchers {

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
                             |:partNum SKU {1} PATTERN \\d{3}-[A-Z]{2} ;
                             |}""".stripMargin.replaceAll(" |\n", ""))
  }

}
