import zio.ZIO
import zio.console.{Console, putStrLn, getStrLn}
import java.io.IOException

object CommandLine:
  def getUserInput(question: String): ZIO[Console, IOException, String] =
    putStrLn(question).flatMap(_ => getStrLn)

  def getUserName: ZIO[Console, IOException, User] =
    for
      firstName <- getUserInput("What is your first name?")
      lastName <- getUserInput("What is your last name?")
      user <- ZIO.fromOption(User.make(firstName, lastName)) <> (putStrLn(
        "Invalid input. Please try again..."
      ) *> getUserName)
    yield user
