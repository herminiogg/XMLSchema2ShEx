# XMLSchema2ShEx
[![Master build](https://github.com/herminiogg/xmlschema2shex/actions/workflows/scala.yml/badge.svg?branch=master)](https://github.com/herminiogg/xmlschema2shex/actions/workflows/scala.yml?query=branch%3Amaster)
[![Maven Central](https://img.shields.io/maven-central/v/com.herminiogarcia/xmlschema2shex_3?color=blue)](https://central.sonatype.com/artifact/com.herminiogarcia/xmlschema2shex_3)

XMLSchema2ShEx is a tool to convert XML Schema files to "equivalent" Shape Expressions files. The goal is to support
the translation from XML data to RDF, preserving the validation rules. It is based in the theoretical paper developed 
in the companion paper (see the [Publications section](#publications))  and implements a subset of the defined
conversions (see the [Supported features section](#supported-features)).

:heavy_exclamation_mark: A experimental conversion to ShExML is being tested in this library letting users to scaffold
the necessary mapping rules in a semi-automatic fashion and then validate the results.

## Supported features
| Supported features | Pending implementation |
|:------------------:|:----------------------:|
| Complex type       | Choice                 |
| Simple type        | List                   |
| All                | Union                  |
| Attributes         | Extension              |
| Restriction        | Fraction Digits        |
| Element            | Length                 |
| Max exclusive      | Max Length             |
| Min exclusive      | Min Length             |
| Max inclusive      | Total digits           |
| Min inclusive      | Whitespace             |
| Enumeration        | Unique                 |
| Pattern            |                        |
| Cardinality        |                        |

## Usage
The library provides two methods of operation: a CLI through the provided JAR package and a JVM compatible API.

### CLI
The CLI can be called using the command `$ java -jar XMLSchema2ShEx.jar [options]` where options are as described below:

```
Usage: XMLSchema2ShEx [-hsV] -i=<file> [-o=<output>]
Convert from XML Schema to ShEx and more...
  -h, --help              Show this help message and exit.
  -i, --input=<file>      Path to XML Schema file
  -o, --output=<output>   Path where the output file should be created
  -s, --shexml            Generate ShExML scaffold
  -V, --version           Print version information and exit.
```

### JVM compatible API
It can also be called from any JVM compatible language that has it as its dependency. The call
should look like similar to the following snippet:

```scala
val shexResult = XMLSchema2ShexParser().parse(input)
val shexmlResult = XMLSchema2ShexParser().convertToShExML(input)
```

## Publications
For citation purposes please use the companion paper:
```
Garcia-Gonzalez, H., & Labra-Gayo, J. E. (2020). XMLSchema2ShEx: 
Converting XML validation to RDF validation. Semantic Web, 11(2), 235-253.
```