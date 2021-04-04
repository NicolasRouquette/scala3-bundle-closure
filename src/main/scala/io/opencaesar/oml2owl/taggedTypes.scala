package io.opencaesar.oml2owl

import java.io.File
import io.github.vigoo.clipp.ParameterParser
import pl.iterators.kebs.tagged.*

object taggedTypes:

  sealed trait ReadableCatalog

  implicit val readableCatalogParser: ParameterParser[File @@ ReadableCatalog] =
    new ParameterParser[File @@ ReadableCatalog]:
      override def parse(value: String): Either[String, File @@ ReadableCatalog] =
        val f = new File(value)
        val v0 = Right(f.taggedWith[ReadableCatalog])
        val v1 = if f.canRead then
          v0
        else
          Left(s"The path must be a readable catalog file, got: $value")
        val v2 = if f.getName == "catalog.xml" then
          v1
        else
          Left(s"The path must be to a readable file named: catalog.xml; got: $value")
        v2

      override def example: File @@ ReadableCatalog =
        new File("catalog.xml").taggedWith[ReadableCatalog]

  sealed trait WriteableCatalog

  implicit val writeableCatalogParser: ParameterParser[File @@ WriteableCatalog] =
    new ParameterParser[File @@ WriteableCatalog]:
      override def parse(value: String): Either[String, File @@ WriteableCatalog] =
        val f = new File(value)
        val v0 = Right(f.taggedWith[WriteableCatalog])
        val v1 = if f.canWrite then
          v0
        else
          val d = f.getParentFile
          if d.isDirectory && d.canExecute then
            v0
          else
            Left(s"The path must be a writeable catalog file in an existing directory, got: $value")
        val v2 = if f.getName == "catalog.xml" then
          v1
        else
          Left(s"The path must be to a readable file named: catalog.xml; got: $value")
        v2

      override def example: File @@ WriteableCatalog =
        new File("catalog.xml").taggedWith[WriteableCatalog]