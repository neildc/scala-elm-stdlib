package elmstdlib

import elmstdlib.Internal.Shortcut
import scala.util.chaining._

import cats.effect.IO

object IOCats {
  def map[A, B](func: (A => B), io: IO[A]): IO[B] =
    Shortcut.map(func, io)

  def map2[A, B, Value](
      func: ((A, B) => Value),
      ioA: IO[A],
      ioB: IO[B]
  ): IO[Value] =
    Shortcut.map2(func, ioA, ioB)

  def map3[A, B, C, Value](
      func: ((A, B, C) => Value),
      ioA: IO[A],
      ioB: IO[B],
      ioC: IO[C]
  ): IO[Value] =
    Shortcut.map3(func, ioA, ioB, ioC)

  def map4[A, B, C, D, Value](
      func: ((A, B, C, D) => Value),
      ioA: IO[A],
      ioB: IO[B],
      ioC: IO[C],
      ioD: IO[D]
  ): IO[Value] =
    Shortcut.map4(func, ioA, ioB, ioC, ioD)

  def map5[A, B, C, D, E, Value](
      func: ((A, B, C, D, E) => Value),
      ioA: IO[A],
      ioB: IO[B],
      ioC: IO[C],
      ioD: IO[D],
      ioE: IO[E]
  ): IO[Value] =
    Shortcut.map5(func, ioA, ioB, ioC, ioD, ioE)

  def andThen[A, B](func: (A => IO[B]), io: IO[A]): IO[B] =
    io.pipe(Shortcut.andThen(func))
}
