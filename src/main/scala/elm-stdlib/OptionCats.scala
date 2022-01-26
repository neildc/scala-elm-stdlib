package elmstdlib

import elmstdlib.Internal.Shortcut
import scala.util.chaining._

// sealed trait Option[A]
// case class Some(a: A) extends Option
// case object None extends Option

object OptionCats {

  /** | Provide a default value, turning an optional value into a normal value.
    * This comes in handy when paired with functions like 'Dict.get' which gives
    * back a @Maybe@.
    *
    * > >>> withDefault 100 (Just 42) > 42 > >>> withDefault 100 Nothing > 100 >
    * >>> withDefault "unknown" (Dict.get "Tom" Dict.empty) > "unknown"
    *
    * __Note:__ This can be overused! Many cases are better handled by a @case@
    * expression. And if you end up using @withDefault@ a lot, it can be a good
    * sign that a [custom
    * type](https://guide.elm-lang.org/types/custom_types.html) will clean your
    * code up quite a bit!
    */
  def withDefault[A](a: A, opt: Option[A]): A =
    opt match {
      case Some(optA) => optA
      case None       => a
    }

  /** Transform a @Maybe@ value with a given function:
    *
    * > >>> map sqrt (Just 9) > Just 3 > >>> map sqrt Nothing > Nothing > >>>
    * map sqrt (String.toFloat "9") > Just 3 > >>> map sqrt (String.toFloat "x")
    * > Nothing
    */

  def map[A, B](func: (A => B), opt: Option[A]): Option[B] =
    Shortcut.map(func, opt)

  /** | Apply a function if all the arguments are @Just@ a value. // > >>> map2
    * (+) (Just 3) (Just 4) > Just 7 > >>> map2 (+) (Just 3) Nothing > Nothing >
    * >>> map2 (+) Nothing (Just 4) > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "123") > Just 124 > >>> map2 (+) (String.toInt "x")
    * (String.toInt "123") > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "1.3") > Nothing
    */
  def map2[A, B, Value](
      func: ((A, B) => Value),
      optA: Option[A],
      optB: Option[B]
  ): Option[Value] =
    Shortcut.map2(func, optA, optB)

  def map3[A, B, C, Value](
      func: ((A, B, C) => Value),
      optA: Option[A],
      optB: Option[B],
      optC: Option[C]
  ): Option[Value] =
    Shortcut.map3(func, optA, optB, optC)

  def map4[A, B, C, D, Value](
      func: ((A, B, C, D) => Value),
      optA: Option[A],
      optB: Option[B],
      optC: Option[C],
      optD: Option[D]
  ): Option[Value] =
    Shortcut.map4(func, optA, optB, optC, optD)

  def map5[A, B, C, D, E, Value](
      func: ((A, B, C, D, E) => Value),
      optA: Option[A],
      optB: Option[B],
      optC: Option[C],
      optD: Option[D],
      optE: Option[E]
  ): Option[Value] =
    Shortcut.map5(func, optA, optB, optC, optD, optE)

  /** | Chain together many computations that may fail. It is helpful to see an
    * This means we only continue with the callback if things are going well.
    * For example, say you need to parse some user input as a month:
    *
    * > parseMonth :: String -> Maybe Int > parseMonth userInput = >
    * String.toInt userInput > |> andThen toValidMonth > > toValidMonth :: Int
    * -> Maybe Int > toValidMonth month = > if 1 <= month && month <= 12 then >
    * Just month > > else > Nothing
    *
    * In the @parseMonth' function, if 'String.toInt@ produces @Nothing@
    * (because the @userInput@ was not an integer) this entire chain of
    * operations will short-circuit and result in @Nothing@. If @toValidMonth@
    * results in
    * @Nothing@,
    *   again the chain of computations will result in @Nothing@.
    *
    * andThen :: (a -> Maybe b) -> Maybe a -> Maybe b andThen = Shortcut.andThen
    */
  def andThen[A, B](func: (A => Option[B]), opt: Option[A]): Option[B] =
    opt.pipe(Shortcut.andThen(func))
}

