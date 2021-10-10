import zio.{ZIO}
import zio.console.{Console, putStrLn}
import java.io.IOException
import CommandLine.getUserInput

object GameEngine:
  def execute(gameState: GameState): ZIO[Console, IOException, Unit] = for {
    guess <- gameState.renderState *> Guess.getGuess
    updatedGameState = GameState.addGuess(guess, gameState)
    _ <- updatedGameState.status match {
      case Status.Won =>
        putStrLn("Congratulations You won!") *> gameState.renderState
      case Status.Lost =>
        putStrLn("Oh no you lost!") *> gameState.renderState
      case Status.Warmer =>
        putStrLn(s"Hmmmm warmer") *> execute(updatedGameState)
      case Status.Colder =>
        putStrLn(s"Oh dear your colder") *> execute(updatedGameState)
      case Status.Unchanged =>
        putStrLn(
          s"You've already tried that number!"
        ) *> execute(gameState)
    }

  } yield ()
