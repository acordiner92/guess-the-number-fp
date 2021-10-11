import zio.{ZIO}
import zio.console.{Console, putStrLn}
import java.io.IOException
import CommandLine.getUserInput

object GameEngine:
  def execute(gameState: GameState): ZIO[Console, IOException, Unit] =
    for
      guess <- gameState.renderState *> Guess.getGuess
      updatedGameState = GameState.addGuess(guess, gameState)
      _ <- updatedGameState.status match
        case Status.Won =>
          putStrLn(
            s"Congratulations ${updatedGameState.user.firstName} you won!"
          ) *> updatedGameState.renderState
        case Status.Lost =>
          putStrLn(
            s"Oh no ${updatedGameState.user.firstName} you lost!"
          ) *> updatedGameState.renderState
        case Status.ReallyWarm =>
          putStrLn(s"getting real warm") *> execute(updatedGameState)
        case Status.Warmer =>
          putStrLn(s"Hm warmer") *> execute(updatedGameState)
        case Status.Colder =>
          putStrLn(s"Oh dear your colder") *> execute(updatedGameState)
        case Status.ReallyCold =>
          putStrLn(s"getting super cold") *> execute(updatedGameState)
        case Status.Unchanged =>
          putStrLn(
            s"${updatedGameState.user.firstName} you have already tried that number!"
          ) *> execute(gameState)
    yield ()
