# Implementation of [OML Bundle Closure](https://github.com/opencaesar/owl-tools/blob/master/owl-close-world/README.md) in Scala3

The goals of this Scala3 implementation are:
- convey a clearer understanding of the algorithm in terms of a simple graph library API sufficient to encode the algorithm;
- add pre/post conditions for verification as a precursor to formalizing the algorithm.

Note: this uses sbt-1.5.0-RC1 and Scala 3.0.0-RC1

On Windows 10, to avoid getting a [null folder](https://github.com/sbt/sbt/issues/5206), set an environment variable: `COURSIER_CACHE=.coursier`.
This will store the Coursier cache in a project local folder: `.coursier`.
