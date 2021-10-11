import zio.{ZIO}
import zio.console.{Console, putStrLn}
import zio.random.{Random, nextIntBounded}
import java.io.IOException

enum Status:
  case ReallyWarm, Warmer, Colder, ReallyCold, Won, Lost, Unchanged

case class GameState(
    user: User,
    numberToGuess: Int,
    guesses: List[Int],
    status: Status,
    guessLimit: Int
) {
  def renderState: ZIO[Console, IOException, Unit] =
    putStrLn {
      s"""
          user: ${user.firstName} ${user.lastName}

          Number of guesses: ${guesses.length}

          guesses: ${guesses.mkString(", ")}

          status: ${status}
          """
    }
}

object GameState {
  def make(user: User): ZIO[Random, Nothing, GameState] =
    nextIntBounded(50).map(numberToGuess =>
      GameState(user, numberToGuess, List(), Status.Unchanged, 10)
    )

  def addGuess(guess: Guess, gameState: GameState): GameState =
    if guess.value == gameState.numberToGuess then
      gameState.copy(
        status = Status.Won,
        guesses = guess.value :: gameState.guesses
      )
    else if gameState.guesses.length >= (gameState.guessLimit - 1) then
      gameState.copy(
        status = Status.Lost,
        guesses = guess.value :: gameState.guesses
      )
    else if gameState.guesses.contains(guess.value) then
      gameState.copy(status = Status.Unchanged)
    else if (guess.value - gameState.numberToGuess).abs < 5 then
      gameState.copy(
        status = Status.ReallyWarm,
        guesses = guess.value :: gameState.guesses
      )
    else if (guess.value - gameState.numberToGuess).abs < 10 then
      gameState.copy(
        status = Status.Warmer,
        guesses = guess.value :: gameState.guesses
      )
    else if (guess.value - gameState.numberToGuess).abs < 20 then
      gameState.copy(
        status = Status.Colder,
        guesses = guess.value :: gameState.guesses
      )
    else
      gameState.copy(
        status = Status.ReallyCold,
        guesses = guess.value :: gameState.guesses
      )
}
