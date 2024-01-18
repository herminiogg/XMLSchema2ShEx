package com.herminiogarcia.xmlschema2shex

import com.herminiogarcia.xmlschema2shex.parser.XMLSchema2ShexParser
import java.io.FileWriter
import java.util.concurrent.Callable
import picocli.CommandLine
import picocli.CommandLine.{Command, Option}

object Main {

  def main(args: Array[String]): Unit = {
    System.exit(new CommandLine(new Main()).execute(args: _*))
  }
}

@Command(name = "XMLSchema2ShEx", version = Array("v0.1.1"),
  mixinStandardHelpOptions = true,
  description = Array("Convert from XML Schema to ShEx and more..."))
class Main extends Callable[Int] {

  @Option(names = Array("-i", "--input"), required = true, description = Array("Path to XML Schema file"))
  private var file: String = ""

  @Option(names = Array("-o", "--output"), description = Array("Path where the output file should be created"))
  private var output: String = ""

  @Option(names = Array("-s", "--shexml"), description = Array("Generate ShExML scaffold"))
  private var shexml: Boolean = false

  override def call(): Int = {
    val fileHandler = scala.io.Source.fromFile(file)
    try {
      val input = fileHandler.mkString
      val result = if (shexml) {
        XMLSchema2ShexParser().convertToShExML(input)
      } else {
        XMLSchema2ShexParser().parse(input, None)
      }
      if (output.nonEmpty) {
        val fw = new FileWriter(output)
        fw.write(result)
        fw.close()
      }
      println(result)
      1
    } finally { fileHandler.close() }
  }

}
