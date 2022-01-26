package elmstdlib.Internal

import cats.Functor
import cats.Applicative
import cats.Monad
import cats.implicits._
import scala.util.chaining._

object Shortcut {
  /** Transform a @Maybe@ value with a given function:
    *
    * > >>> map sqrt (Just 9) > Just 3 > >>> map sqrt Nothing > Nothing > >>>
    * map sqrt (String.toFloat "9") > Just 3 > >>> map sqrt (String.toFloat "x")
    * > Nothing
    */

  def map[F[_]: Functor, A, B](func: ((A => B)), fa: F[A]): F[B] = {
    fa.map(func)
  }
  /** | Apply a function if all the arguments are @Just@ a value. // > >>> map2
    * (+) (Just 3) (Just 4) > Just 7 > >>> map2 (+) (Just 3) Nothing > Nothing >
    * >>> map2 (+) Nothing (Just 4) > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "123") > Just 124 > >>> map2 (+) (String.toInt "x")
    * (String.toInt "123") > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "1.3") > Nothing
    */

  def map2[A, B, Value, F[_]: Applicative](
      func: ((A, B) => Value),
      fA: F[A],
      fB: F[B]
  ): F[Value] = {
    (fA, fB).mapN(func)
  }

  def map3[A, B, C, Value, F[_]: Applicative](
      func: ((A, B, C) => Value),
      fA: F[A],
      fB: F[B],
      fC: F[C]
  ): F[Value] =
    (fA, fB, fC).mapN(func)

  def map4[L, A, B, C, D, Value, F[_]: Applicative](
      func: ((A, B, C, D) => Value),
      fA: F[A],
      fB: F[B],
      fC: F[C],
      fD: F[D]
  ): F[Value] =
    (fA, fB, fC, fD).mapN(func)

  def map5[L, A, B, C, D, E, Value, F[_]: Applicative](
      func: ((A, B, C, D, E) => Value),
      fA: F[A],
      fB: F[B],
      fC: F[C],
      fD: F[D],
      fE: F[E]
  ): F[Value] =
    (fA, fB, fC, fD, fE).mapN(func)

  // /** | Chain together many computations that may fail. It is helpful to see an
  //   * This means we only continue with the callback if things are going well.
  //   * For elmstdlib, say you need to parse some user input as a month:
  //   *
  //   * > parseMonth :: String -> Maybe Int > parseMonth userInput = >
  //   * String.toInt userInput > |> andThen toValidMonth > > toValidMonth :: Int
  //   * -> Maybe Int > toValidMonth month = > if 1 <= month && month <= 12 then >
  //   * Just month > > else > Nothing
  //   *
  //   * In the @parseMonth' function, if 'String.toInt@ produces @Nothing@
  //   * (because the @userInput@ was not an integer) this entire chain of
  //   * operations will short-circuit and result in @Nothing@. If @toValidMonth@
  //   * results in
  //   * @Nothing@,
  //   *   again the chain of computations will result in @Nothing@.
  //   *
  //   * andThen :: (a -> Maybe b) -> Maybe a -> Maybe b andThen = Shortcut.andThen
  //   */

  def andThen[A, B, F[_]: Monad](func: (A => F[B]))( fa: F[A]): F[B] =
    fa.flatMap(func)
}
