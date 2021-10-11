import scala.util.control.Exception.*
import zio.{ZIO}
import zio.console.{Console, putStrLn}
import java.io.IOException
import CommandLine.getUserInput

sealed abstract case class Guess private (value: Int)

object Guess:
  def make(value: String): Option[Guess] =
    allCatch.opt(new Guess(value.toInt) {})

  def getGuess: ZIO[Console, IOException, Guess] =
    for
      userGuessInput <- getUserInput("Which number do you guess?")
      guess <- ZIO.fromOption(Guess.make(userGuessInput)) <> (putStrLn(
        "Your input is not a valid number"
      ) *> getGuess)
    yield guess
