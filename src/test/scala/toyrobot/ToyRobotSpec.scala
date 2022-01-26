package toyrobot

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try
import elmstdlib.EitherSimple
import elmstdlib.EitherCats
import scala.util.chaining._

// Ideally this is opaque... but out of scope for this talk
case class State (position: Position, direction: Direction)

// Would be better if these were unsigned...
final case class Position(x: Int, y: Int)

// assume this is an Enum
// object Direction extends Enumeration {
//    val North, East, West, South = Value;
// }
sealed abstract class Direction
object Direction {
  final case object North extends Direction
  final case object East extends Direction
  final case object South extends Direction
  final case object West extends Direction
}

// final case object => static
// Move
// Command.Move
sealed abstract class Command
object Command {
  final case class Place(position: Position, direction: Direction) extends Command
  final case object Move extends Command
  final case object RotateLeft extends Command
  final case object RotateRight extends Command
}

sealed abstract class ParseError
final case class APlaceParseError(p: PlaceParseError) extends ParseError
final case object InvalidCommand extends ParseError

sealed abstract class PlaceParseError
final case class InvalidFormat(originalInput: String) extends PlaceParseError
final case class InvalidDirection(attemptedDirStr: String) extends PlaceParseError
final case class APositionParseError(e: PositionParseError) extends PlaceParseError

sealed abstract class PositionParseError
final case class PositionNotInt(attemptedPosStr: String) extends PositionParseError
final case class PositionOutOfBounds(coord: Int) extends PositionParseError

object ToyRobot {
  val BOARD_X_MAX = 4
  val BOARD_Y_MAX = 4

  def positionAtOrigin = Position(0, 0)

  def execute(command: Command, state: State): State =  {
    command match {
      case Command.Place(position, direction) =>
        State(position, direction)

      case Command.RotateLeft =>
        state.copy(direction = rotateLeft(state.direction))

      case Command.RotateRight =>
        state.copy(direction = rotateRight(state.direction))

      case Command.Move =>
        moveIfRobotWontFall(state) match {
          case Some(newPosition) => state.copy(position = newPosition)
          case None              => state
        }
    }
  }

  private def rotateLeft(dir: Direction): Direction = {
    dir match {
      case Direction.North => Direction.West
      case Direction.East  => Direction.North
      case Direction.South => Direction.East
      case Direction.West  => Direction.South
    }
  }

  private def rotateRight(dir: Direction): Direction = {
    dir match {
      case Direction.North => Direction.East
      case Direction.East  => Direction.South
      case Direction.South => Direction.West
      case Direction.West  => Direction.North
    }
  }

  private def moveIfRobotWontFall(state: State): Option[Position] = {
    val (facingBoundary, newPosition) = {
      state.direction match {
        case Direction.North =>
          (
            state.position.y == BOARD_X_MAX,
            state.position.copy(y = state.position.y + 1)
          )

        case Direction.East =>
          (
            state.position.x == BOARD_Y_MAX,
            state.position.copy(x = state.position.x + 1)
          )

        case Direction.South =>
          (state.position.y == 0, state.position.copy(y = state.position.y - 1))

        case Direction.West =>
          (state.position.x == 0, state.position.copy(x = state.position.x - 1))
      }
    }

    if (facingBoundary) {
      None
    } else {
      Some(newPosition)
    }
  }

  def parseCommand(input: String): Either[ParseError, Command] = {
    input match {
      case "MOVE"  => Right(Command.Move)
      case "LEFT"  => Right(Command.RotateLeft)
      case "RIGHT" => Right(Command.RotateRight)
      case _ =>
        input.split(" ") match {
          // PLACE X,Y,DIR
          case Array("PLACE", placeArgs) =>

            parsePlaceCommand(placeArgs) match {
              case Right((position, direction)) =>
                Right(Command.Place(position, direction))

              case Left(err) => Left(APlaceParseError(err))
            }

          case _ =>
            Left(InvalidCommand)
        }
    }
  }

  def parsePlaceCommandPatternMatch(input: String): Either[PlaceParseError, (Position, Direction)] = {
    splitIntoThree(input) match {
      case Left(e) => Left(e)
      case Right((xStr, yStr, dirStr))  => {
        val parseX = parsePosition(xStr, 0, BOARD_X_MAX)
        val parseY = parsePosition(yStr, 0, BOARD_Y_MAX)
        val parseDir = parseDirection(dirStr)

        // return (Right(State(Position(x,y), dir)))

        (parseX, parseY, parseDir) match {
          case (Right(x), Right(y), Right(dir)) => Right((Position(x,y), dir))
          case (Left(parseXErr), _, _)          => Left(APositionParseError(parseXErr))
          case (_,  Left(parseYErr), _)         => Left(APositionParseError(parseYErr))
          case (_,  _, Left(parseDirErr))       => Left(parseDirErr)
        }
      }
    }
  }

  def parsePlaceCommandMap3Simple(input: String): Either[PlaceParseError, (Position, Direction)] = {

    def f (validX: Int, validY: Int, validDir: Direction): (Position, Direction)  =
      (Position(validX,validY), validDir)

    splitIntoThree(input) match {
      case Left(e) => Left(e)
      case Right((xStr, yStr, dirStr)) => 
        EitherSimple.map3(
          f,
          parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError),
          parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError),
          parseDirection(dirStr)
        )
    }
  }

  def parsePlaceCommandMap3Cats(input: String): Either[PlaceParseError, (Position, Direction)] = {

      def f (validX: Int, validY: Int, validDir: Direction): (Position, Direction)  =
        (Position(validX,validY), validDir)

      splitIntoThree(input) match {
        case Left(e) => Left(e)
        case Right((xStr, yStr, dirStr)) => 
          EitherCats.map3(
            f,
            parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError),
            parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError),
            parseDirection(dirStr)
          )
      }
    }

  def parsePlaceCommandFlatMap(input: String): Either[PlaceParseError, (Position, Direction)] = {

    splitIntoThree(input).flatMap(validStringTriplet => {

      val (xStr, yStr, dirStr) = validStringTriplet;

      parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError)
        .flatMap (x =>
            parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError)
              .flatMap(y =>
                parseDirection(dirStr).flatMap(dir =>
                  Right((Position(x, y), dir))
               )
            )
         )
      }
    )
  }

  def parsePlaceCommandAndThen(input: String): Either[PlaceParseError, (Position, Direction)] = {

      splitIntoThree(input).pipe(EitherSimple.andThen(validStringTriplet => {
        val (xStr, yStr, dirStr) = validStringTriplet;

        parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError)
          .pipe(EitherSimple.andThen(x =>
              parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError)
                .pipe(EitherSimple.andThen(y =>
                  parseDirection(dirStr)
                    .pipe(EitherSimple.andThen((dir => Right((Position(x, y), dir))))
                  )
                ))
              ))
        })
      )
    }

  def parsePlaceCommandAndThenNoPipe(input: String): Either[PlaceParseError, (Position, Direction)] = {
    // def splitF(xStr: String, yStr: String, dirStr: String): Either[PlaceParseError, (String, String, String)] =
    //   Right((xStr, yStr, dirStr))

    // val splitRes: Either[PlaceParseError, (String, String, String)] = EitherSimple.andThen((xStr, yStr, dirStr) => splitF(xStr, yStr,dirStr))(splitIntoThree(input))
    splitIntoThree(input).flatMap(validStringTriplet => {

      val (xStr, yStr, dirStr) = validStringTriplet;

      val eitherX: Either[PlaceParseError, Int] = parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError)
      eitherX match {
        case Left(err) => Left(err)
        case Right(x) => {
          val eitherY: Either[PlaceParseError, Int] = parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError)
          eitherY match {
            case Left(err) => Left(err)
            case Right(y) => {

              def dirF(dir: Direction): Either[PlaceParseError, (Position, Direction)] = Right((Position(x, y), dir))
              EitherSimple.andThen(dirF)(parseDirection(dirStr))
            }

          }
        }

      }
    })
  }

  def parsePlaceCommand(input: String): Either[PlaceParseError, (Position, Direction)] = {
    /**************************************
             for comprehension
    **************************************/

    // don't of this as a for loop (no relationship)
    for (
      validStringTriplet <- splitIntoThree(input);
      (xStr, yStr, dirStr) = validStringTriplet;

      x <- parsePosition(xStr, 0, BOARD_X_MAX).left.map(APositionParseError); // TODO cleaner mapLeft?
      y <- parsePosition(yStr, 0, BOARD_Y_MAX).left.map(APositionParseError); // TODO cleaner mapLeft

      direction <- parseDirection(dirStr)

    ) yield (Position(x,y), direction)
  }

  private def splitIntoThree(s: String): Either[InvalidFormat, (String, String, String)] = {
      s.split(",") match {
        case Array(xStr, yStr, dirStr) => Right((xStr, yStr, dirStr))
        case _                         => Left(InvalidFormat(s))
      }
    }

  private def parseDirection(s: String): Either[InvalidDirection, Direction] = {
      s match {
        case "NORTH" => Right(Direction.North)
        case "EAST"  => Right(Direction.East)
        case "WEST"  => Right(Direction.West)
        case "SOUTH" => Right(Direction.South)
        case _       => Left(InvalidDirection(s))
      }
    }
  private def parsePosition(positionInput: String, minVal: Int, maxVal: Int):
      Either[PositionParseError, Int] = {

    def stringToMaybeInt(s: String): Either[PositionNotInt, Int] = {
      Try(s.toInt).toOption match {
        case Some(n) => Right(n)
        case None    => Left(PositionNotInt(s))
      }
    }

    def isInValidRange(value: Int): Either[PositionOutOfBounds, Int] = {
      if (value >= minVal && value <= maxVal ) {
        // null
        Right(value)
      } else {
        // Left(PositionNotInt(value))
        // Left(PositionNotInt("AAA"))
        Left(PositionOutOfBounds(value))
      }
    }

    // for {
    //   int <- stringToMaybeInt(input)
    //   _ <- isInValidRange(int)
    // } yield(int)
    stringToMaybeInt(positionInput).flatMap(isInValidRange)
  }
}


class ToyRobotSpec extends AnyFlatSpec with Matchers {
  def testAllParsePlaceCommands(input: String, output: Either[PlaceParseError, (Position, Direction)]) =  {
    ToyRobot.parsePlaceCommand(input) shouldEqual output
    ToyRobot.parsePlaceCommandPatternMatch(input) shouldEqual output
    ToyRobot.parsePlaceCommandMap3Simple(input) shouldEqual output
    ToyRobot.parsePlaceCommandMap3Cats(input) shouldEqual output
    ToyRobot.parsePlaceCommandFlatMap(input) shouldEqual output
    ToyRobot.parsePlaceCommandAndThen(input) shouldEqual output
    ToyRobot.parsePlaceCommandAndThenNoPipe(input) shouldEqual output
  }

  "placeParse*" should "fail on invalid input" in {
    val input = "AAAA"
    val output = Left(InvalidFormat(input))
    testAllParsePlaceCommands(input, output)
  }
  it should "check for commas" in {
    val input = "3=3=NORTH"
    val output = Left(InvalidFormat(input))
    testAllParsePlaceCommands(input, output)
  }

  it should "parse correctly" in {
    val input = "3,3,NORTH"
    val output = Right((Position(3,3), Direction.North))
    testAllParsePlaceCommands(input, output)
  }

  it should "require valid ints (x)" in {
    val input = "x,3,NORTH"
    val output = Left(APositionParseError(PositionNotInt("x")))
    testAllParsePlaceCommands(input, output)
  }

  it should "require valid ints (y)" in {
     val input = "3,y,NORTH"
     val output = Left(APositionParseError(PositionNotInt("y")))
     testAllParsePlaceCommands(input, output)
  }

  it should "check that the position is in bounds (x)" in {
    val input = "9,0,NORTH"
    val output = Left(APositionParseError(PositionOutOfBounds(9)))
    testAllParsePlaceCommands(input, output)
  }

  it should "check that the position is in bounds (y)" in {
    val input = "2,7,NORTH"
    val output = Left(APositionParseError(PositionOutOfBounds(7)))
    testAllParsePlaceCommands(input, output)
  }

  it should "check that the direction is valid" in {
    val input = "2,2,AAAA"
    val output = Left(InvalidDirection("AAAA"))
    testAllParsePlaceCommands(input, output)
  }
}
