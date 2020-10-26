package es.weso.xmlschema2shex.inputoutputtests

import es.weso.xmlschema2shex.parser.XMLSchema2ShexParser
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/**
  * Created by herminio on 19/12/16.
  */
class ShExGenerationTests extends AnyFunSuite with Matchers {

  test("XML Schema conversion from Microsoft example") {
    val xml = Source.fromResource("purchaseOrderFull.xsd").mkString
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
    val xml = Source.fromResource("address.xsd").mkString
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
    val xml = Source.fromResource("xml1version.xsd").mkString
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
    val xml = Source.fromResource("xml2version.xsd").mkString
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
