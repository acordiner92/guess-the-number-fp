import zio.*
import zio.console.{putStrLn}
import CommandLine.getUserName

object Main extends zio.App:
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for
      user <- putStrLn("Welcome to guess the number") *> getUserName
      newGameState <- GameState.make(user)
      _ <- GameEngine.execute(newGameState)
    yield ()).exitCode
