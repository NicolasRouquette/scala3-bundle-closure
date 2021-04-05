# Implementation of [OML Bundle Closure](https://github.com/opencaesar/owl-tools/blob/master/owl-close-world/README.md) in Scala3

This is a short-term workaround to some problems with using scala 2.13 libraries with scala 3:

https://github.com/vigoo/clipp/issues/194
https://users.scala-lang.org/t/an-existential-type-that-came-from-a-scala-2-classfile-cannot-be-mapped-accurately-to-to-a-scala-3-equivalent/7309

I managed to avoid these problems by copying the necessary source 
code and using scala3.0.0-RC2 libraries only:
- [clipp using cats effects](https://github.com/vigoo/clipp)
- [kebs tagged types](https://github.com/theiterators/kebs)

The goals of this Scala3 implementation are:
- convey a clearer understanding of the algorithm in terms of a simple graph library API sufficient to encode the algorithm;
- add pre/post conditions for verification as a precursor to formalizing the algorithm.

Note: this uses sbt-1.5.0-RC1 and Scala 3.0.0-RC2


## Running

- Download this example of OML ontologies (vocabularies and descriptions)

  https://github.com/modelware/oml-tutorial

- In that example, run a conversion from OML to Owl, e.g.:

  ```shell
  ./gradlew :pattern1:omlToOwl -info # unix
  .\gradlew :pattern1:omlToOwl -info # windows powershell
  ```
  
- Pick one of the vocabulary or description bundle IRI
- In this project's sbt shell:
  
  ```sbt
  run -i <input catalog path> -o <output catalog path> -r <root bundle iri>
  ```
  
Caveat: this is work in progres...
