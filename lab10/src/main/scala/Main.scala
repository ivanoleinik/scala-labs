import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.effect.concurrent.MVar
import cats.syntax.all._

import scala.concurrent.duration._

object Main extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("printer is closed")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Long): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put(counter.toString)
      _ <- rec(counter + 1)
    } yield ()

    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("counter is closed")))).void
  }

  val gracefulShutdownProgram: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(print("release")))
    _ <- runCounter(mvar)
    _ <- runPrinter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    gracefulShutdownProgram.use(_ => IO.never)
}