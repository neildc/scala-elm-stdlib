package elmstdlib

import elmstdlib.Internal.Shortcut

// sealed trait Either[L,E,A]
// case class Right(a: A/ extends Either[L,Nothing,A]
// case class Left(e:E) extends Either[L,E,Nothing]

object EitherCats {

  /** */
  def withDefault[L, A](a: A, eth: Either[L, A]): A =
    eth match {
      case Right(ethA) => ethA
      case Left(e)     => a
    }

  /** Transform a @Maybe@ value with a given function:
    *
    * > >>> map sqrt (Just 9) > Just 3 > >>> map sqrt Nothing > Nothing > >>>
    * map sqrt (String.toFloat "9") > Just 3 > >>> map sqrt (String.toFloat "x")
    * > Nothing
    */
  def map[L, A, B](func: (A => B), eth: Either[L, A]): Either[L, B] =
    Shortcut.map(func, eth)

  /** | Apply a function if all the arguments are @Just@ a value. // > >>> map2
    * (+) (Just 3) (Just 4) > Just 7 > >>> map2 (+) (Just 3) Nothing > Nothing >
    * >>> map2 (+) Nothing (Just 4) > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "123") > Just 124 > >>> map2 (+) (String.toInt "x")
    * (String.toInt "123") > Nothing > >>> map2 (+) (String.toInt "1")
    * (String.toInt "1.3") > Nothing
    */
  def map2[L, A, B, Value](
        func: ((A, B) => Value),
        ethA: Either[L, A],
        ethB: Either[L, B],
    ): Either[L, Value] =
      Shortcut.map2(func, ethA, ethB)

  def map3[L, A, B, C, Value](
      func: ((A, B, C) => Value),
      ethA: Either[L, A],
      ethB: Either[L, B],
      ethC: Either[L, C]
  ): Either[L, Value] =
    Shortcut.map3(func, ethA, ethB, ethC)


  def map4[L, A, B, C, D, Value](
      func: ((A, B, C, D) => Value),
      ethA: Either[L, A],
      ethB: Either[L, B],
      ethC: Either[L, C],
      ethD: Either[L, D]
  ): Either[L, Value] =
    Shortcut.map4(func, ethA, ethB, ethC, ethD)

  def map5[L, A, B, C, D, E, Value](
      func: ((A, B, C, D, E) => Value),
      ethA: Either[L, A],
      ethB: Either[L, B],
      ethC: Either[L, C],
      ethD: Either[L, D],
      ethE: Either[L, E]
  ): Either[L, Value] =
    Shortcut.map5(func, ethA, ethB, ethC, ethD, ethE)

  /** | Chain together many computations that may fail. It is helpful to see an
    * This means we only continue with the callback if things are going well.
    * For scala-elm-stblib, say you need to parse some user input as a month:
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
  def andThen[L, A, B](func: (A => Either[L, B]))(eth: Either[L, A]): Either[L, B] =
    Shortcut.andThen(func)(eth)
}
