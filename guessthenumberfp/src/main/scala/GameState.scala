import zio.{ZIO}
import zio.console.{Console, putStrLn}
import zio.random.{Random, nextIntBounded}
import java.io.IOException

enum Status:
  case Warmer, Colder, Won, Lost, Unchanged

case class GameState(
    numberToGuess: Int,
    guesses: List[Int],
    status: Status,
    guessLimit: Int
) {
  def renderState: ZIO[Console, IOException, Unit] =
    putStrLn {
      s"""
          Number of guesses: ${guesses.length}

          guesses: ${guesses.mkString(", ")}

          status: ${status}
          """
    }
}

object GameState {
  def make: ZIO[Random, Nothing, GameState] =
    nextIntBounded(50).map(numberToGuess =>
      GameState(numberToGuess, List(), Status.Unchanged, 10)
    )

  def addGuess(guess: Guess, gameState: GameState): GameState =
    if guess.value == gameState.numberToGuess then
      gameState.copy(status = Status.Won)
    else if gameState.guesses.length >= gameState.guessLimit then
      gameState.copy(status = Status.Lost)
    else if gameState.guesses.contains(guess) then
      gameState.copy(status = Status.Unchanged)
    else if (guess.value - gameState.numberToGuess).abs < 20 then
      gameState.copy(
        status = Status.Warmer,
        guesses = guess.value :: gameState.guesses
      )
    else
      gameState.copy(
        status = Status.Colder,
        guesses = guess.value :: gameState.guesses
      )
}
