package com.herminiogarcia.xmlschema2shex.inputoutputtests

import com.herminiogarcia.xmlschema2shex.parser.XMLSchema2ShexParser
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/**
  * Created by herminio on 19/12/16.
  */
class ShExMLGenerationTests extends AnyFunSuite with Matchers {

  test("Xml1 version") {
    val xml = Source.fromResource("xml1version.xsd").mkString
    val output = XMLSchema2ShexParser().convertToShExML(xml).stripMargin.replaceAll("\\s", "")
    output should include ("""PREFIX : <http://example.com/>
                             |PREFIX xs: <http://www.w3.org/2001/XMLSchema#>
                             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>""".stripMargin.replaceAll("\\s", ""))
    output should include ("""
                             |SOURCE example <http://example.com/example.xml>
                             |""".stripMargin.replaceAll("\\s", ""))
    output should include ("""ITERATOR shipto <xpath: /shipto> {
                             |	FIELD name <name>
                             |	FIELD address <address>
                             |	FIELD city <city>
                             |	FIELD country <country>
                             |}
                             |ITERATOR item <xpath: /item> {
                             |	FIELD title <title>
                             |	FIELD note <note>
                             |	FIELD quantity <quantity>
                             |	FIELD price <price>
                             |}
                             |ITERATOR shiporder <xpath: /shiporder> {
                             |	FIELD orderid <@orderid>
                             |	FIELD orderperson <orderperson>
                             |	ITERATOR shipto <shipto> {
                             |		FIELD name <name>
                             |		FIELD address <address>
                             |		FIELD city <city>
                             |		FIELD country <country>
                             |	}
                             |	ITERATOR item <item> {
                             |		FIELD title <title>
                             |		FIELD note <note>
                             |		FIELD quantity <quantity>
                             |		FIELD price <price>
                             |	}
                             |}
                             |EXPRESSION exp <example.shipto>
                             |AUTOINCREMENT subjectAutoincrementId <"subject_" + 1 to 2147483647 by 1>""".stripMargin.replaceAll("\\s", ""))
    output should include (""":shiporder :[exp.orderid] {
                             |	:orderperson [exp.orderperson] xs:string ;
                             |	:orderid [exp.orderid] xs:string ;
                             |  :shipto @:shipto ;
                             |	:item @:item ;
                             |}
                             |:item :[subjectAutoincrementId] {
                             |	:title [exp.item.title] xs:string ;
                             |	:note [exp.item.note] xs:string ;
                             |	:quantity [exp.item.quantity] xs:positiveInteger ;
                             |	:price [exp.item.price] xs:decimal ;
                             |}
                             |:shipto :[subjectAutoincrementId] {
                             |	:name [exp.shipto.name] xs:string ;
                             |	:address [exp.shipto.address] xs:string ;
                             |	:city [exp.shipto.city] xs:string ;
                             |	:country [exp.shipto.country] xs:string ;
                             |}""".stripMargin.replaceAll("\\s", ""))
  }

  test("""Xml2 version""") {
    val xml = Source.fromResource("xml2version.xsd").mkString
    val output = XMLSchema2ShexParser().convertToShExML(xml).stripMargin.replaceAll("\\s", "")
    output should include ("""PREFIX : <http://example.com/>
                             |PREFIX xs: <http://www.w3.org/2001/XMLSchema#>
                             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>""".stripMargin.replaceAll("\\s", ""))
    output should include ("""
                             |SOURCE example <http://example.com/example.xml>
                             |""".stripMargin.replaceAll("\\s", ""))
    output should include ("""ITERATOR shiporder <xpath: /shiporder> {
                             |	FIELD orderid <@orderid>
                             |	FIELD orderperson <orderperson>
                             |	ITERATOR shipto <shipto> {
                             |		FIELD name <name>
                             |		FIELD address <address>
                             |		FIELD city <city>
                             |		FIELD country <country>
                             |	}
                             |	ITERATOR item <item> {
                             |		FIELD title <title>
                             |		FIELD note <note>
                             |		FIELD quantity <quantity>
                             |		FIELD price <price>
                             |	}
                             |}
                             |EXPRESSION exp <example.shiporder>
                             |AUTOINCREMENT subjectAutoincrementId <"subject_" + 1 to 2147483647 by 1>""".stripMargin.replaceAll("\\s", ""))
    output should include (""":shiporder :[exp.orderid] {
                             |	:orderperson [exp.orderperson] xs:string ;
                             |	:orderid [exp.orderid] xs:string ;
                             |  :shipto @:shipto ;
                             |	:item @:item ;
                             |}
                             |:item :[subjectAutoincrementId] {
                             |	:title [exp.item.title] xs:string ;
                             |	:note [exp.item.note] xs:string ;
                             |	:quantity [exp.item.quantity] xs:positiveInteger ;
                             |	:price [exp.item.price] xs:decimal ;
                             |}
                             |:shipto :[subjectAutoincrementId] {
                             |	:name [exp.shipto.name] xs:string ;
                             |	:address [exp.shipto.address] xs:string ;
                             |	:city [exp.shipto.city] xs:string ;
                             |	:country [exp.shipto.country] xs:string ;
                             |}""".stripMargin.replaceAll("\\s", ""))
  }

  test("XML Schema conversion for addresses") {
    val xml = Source.fromResource("address.xsd").mkString
    val output = XMLSchema2ShexParser().convertToShExML(xml).stripMargin.replaceAll("\\s", "")
    output should include ("""PREFIX : <http://example.com/>
                             |PREFIX xs: <http://www.w3.org/2001/XMLSchema#>
                             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>""".stripMargin.replaceAll("\\s", ""))
    output should include ("""
                             |SOURCE example <http://example.com/example.xml>
                             |""".stripMargin.replaceAll("\\s", ""))
    output should include ("""ITERATOR addresses <xpath: /addresses> {
                             |	ITERATOR address <address> {
                             |		FIELD name <name>
                             |		FIELD street <street>
                             |	}
                             |}
                             |ITERATOR address <xpath: /address> {
                             |	FIELD name <name>
                             |	FIELD street <street>
                             |}
                             |EXPRESSION exp <example.addresses>
                             |AUTOINCREMENT subjectAutoincrementId <"subject_" + 1 to 2147483647 by 1>""".stripMargin.replaceAll("\\s", ""))
    output should include (""":addresses :[subjectAutoincrementId] {
                             |	:address @:address ;
                             |}
                             |:address :[subjectAutoincrementId] {
                             |	:name [exp.name] xs:string ;
                             |	:street [exp.street] xs:string ;
                             |}""".stripMargin.replaceAll("\\s", ""))
  }

  test("""XML Schema conversion from Microsoft example""") {
    val xml = Source.fromResource("purchaseOrderFull.xsd").mkString
    val output = XMLSchema2ShexParser().convertToShExML(xml).stripMargin.replaceAll("\\s", "")
    output should include ("""PREFIX : <http://example.com/>
                             |PREFIX xs: <http://www.w3.org/2001/XMLSchema#>
                             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                             |PREFIX tn: <http://tempuri.org/po.xsd>""".stripMargin.replaceAll("\\s", ""))
    output should include ("""
                             |SOURCE example <http://example.com/example.xml>
                             |""".stripMargin.replaceAll("\\s", ""))
    output should include ("""ITERATOR purchaseOrder <xpath: /purchaseOrder> {
                             |	FIELD orderDate <@orderDate>
                             |	FIELD comment <comment>
                             |	ITERATOR shipTo <shipTo> {
                             |		FIELD country <@country>
                             |		FIELD name <name>
                             |		FIELD street <street>
                             |		FIELD city <city>
                             |		FIELD state <state>
                             |		FIELD zip <zip>
                             |	}
                             |	ITERATOR billTo <billTo> {
                             |		FIELD country <@country>
                             |		FIELD name <name>
                             |		FIELD street <street>
                             |		FIELD city <city>
                             |		FIELD state <state>
                             |		FIELD zip <zip>
                             |	}
                             |	ITERATOR items <items> {
                             |		ITERATOR item <item> {
                             |			FIELD partNum <@partNum>
                             |			FIELD productName <productName>
                             |			FIELD quantity <quantity>
                             |			FIELD USPrice <USPrice>
                             |			FIELD comment <comment>
                             |			FIELD shipDate <shipDate>
                             |		}
                             |	}
                             |}
                             |EXPRESSION exp <example.purchaseOrder>
                             |AUTOINCREMENT subjectAutoincrementId <"subject_" + 1 to 2147483647 by 1>""".stripMargin.replaceAll("\\s", ""))
    output should include ("""tn:purchaseOrder tn:[subjectAutoincrementId] {
                             |	tn:comment [exp.comment] xs:string ;
                             |	tn:orderDate [exp.orderDate] xs:date ;
                             |  tn:shipTo @tn:shipTo ;
                             |	tn:billTo @tn:billTo ;
                             | 	tn:items @tn:items ;
                             |}
                             |tn:items tn:[subjectAutoincrementId] {
                             |	tn:item @tn:item ;
                             |}
                             |tn:item tn:[subjectAutoincrementId] {
                             |	tn:productName [exp.items.item.productName] xs:string ;
                             |	tn:quantity [exp.items.item.quantity] xs:positiveInteger ;
                             |	tn:USPrice [exp.items.item.USPrice] xs:decimal ;
                             |	tn:comment [exp.items.item.comment] xs:string ;
                             |	tn:shipDate [exp.items.item.shipDate] xs:date ;
                             |	tn:partNum [exp.items.item.partNum] xs:string ;
                             |}
                             |tn:billTo tn:[subjectAutoincrementId] {
                             |	tn:name [exp.billTo.name] xs:string ;
                             |	tn:street [exp.billTo.street] xs:string ;
                             |	tn:city [exp.billTo.city] xs:string ;
                             |	tn:state [exp.billTo.state] xs:string ;
                             |	tn:zip [exp.billTo.zip] xs:decimal ;
                             |	tn:country tn:[exp.billTo.country]  ;
                             |}
                             |tn:shipTo tn:[subjectAutoincrementId] {
                             |	tn:name [exp.shipTo.name] xs:string ;
                             |	tn:street [exp.shipTo.street] xs:string ;
                             |	tn:city [exp.shipTo.city] xs:string ;
                             |	tn:state [exp.shipTo.state] xs:string ;
                             |	tn:zip [exp.shipTo.zip] xs:decimal ;
                             |	tn:country tn:[exp.shipTo.country]  ;
                             |}""".stripMargin.replaceAll("\\s", ""))
  }




}
