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
    val xml = """
         <xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>


 <xs:element name="addresses">
 <xs:complexType>
 <xs:sequence>
 <xs:element ref="address" minOccurs='1' maxOccurs='unbounded'/>
 </xs:sequence>
 </xs:complexType>
</xs:element>

 <xs:element name="address">
 <xs:complexType>
 <xs:sequence>
 <xs:element ref="name" minOccurs='0' maxOccurs='1'/>
 <xs:element ref="street" minOccurs='0' maxOccurs='1'/>
 </xs:sequence>
 </xs:complexType>
 </xs:element>

 <xs:element name="name" type='xs:string'/>
 <xs:element name="street" type='xs:string'/>
</xs:schema>
      """

    val schema = parseAll(root, xml).get
    new SemanticChecker(schema).check()
    val decoratedSchema = new TypeDecorator(new NameDecorator(schema).decorate()).decorate()
    println(decoratedSchema)
    val fw = new FileWriter("result.shex")
    fw.write(new CodeGenerator(decoratedSchema).generate())
    fw.close()
    println(new CodeGenerator(decoratedSchema).generate())
  }

}