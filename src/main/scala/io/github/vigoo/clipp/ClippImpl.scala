package io.github.vigoo.clipp

import cats.free.Free

import scala.language.higherKinds

trait ClippImpl[F[_]] {
  def parseOrFail[T](args: Seq[String], spec: Parameter.Spec[T])
                    (implicit cio: ClippIO[F]): F[T] = {
    Parser.extractParameters(args, spec) match {
      case Left(parserFailure) =>
        ClippIO[F].failWith(parserFailure)
      case Right(result) =>
        ClippIO[F].succeed(result)
    }
  }

  def displayErrors(failure: ParserFailure)
                   (implicit cio: ClippIO[F]): F[Unit] = {
    ClippIO[F].showErrors(errors.display(failure.errors))
  }

  def parseOrDisplayErrors[T, Res](args: Seq[String], spec: Parameter.Spec[T], errorResult: Res)
                                  (f: T => F[Res])
                                  (implicit cio: ClippIO[F]): F[Res] = {
    ClippIO[F].recoverWith(
      ClippIO[F].flatMap(parseOrFail(args, spec))(f))(failure => ClippIO[F].flatMap(displayErrors(failure))(_ => ClippIO[F].succeed(errorResult)))
  }

}
