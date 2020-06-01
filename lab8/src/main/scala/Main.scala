import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.{Applicative, Id, Monad}
import cats.instances.list._
import cats.syntax.all._

import scala.jdk.CollectionConverters._
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait MvFile[F[_], Dir, File] {
  def mvFile(file: File, dir: Dir): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait GetName[F[_], File] {
  def getName(file: File): F[String]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               mvFile: MvFile[F, Dir, File],
                               printer: Printer[F, File],
                               getFiles: GetFiles[F, Dir, File],
                               getName: GetName[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getFiles.getFiles(testDir)
    _ <- files.traverse(file => printer.printName(file))
    names <- files.traverse(file => getName.getName(file))
    letters = names.map(_.head)
    dirs <- letters.traverse(char => mkDir.mkDir(testDir, char.toString))
    _ <- files.zip(dirs).traverse(pair => mvFile.mvFile(pair._1, pair._2))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path]
  with MkFile[F, Path, Path]
  with MvFile[F, Path, Path]
  with GetFiles[F, Path, Path]
  with GetName[F, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def mvFile(file: Path, dir: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]

  override def getFiles(dir: Path): F[List[Path]] =
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]

  override def getName(file: Path): F[String] =
    file.getFileName.toString.pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object Main {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."))
  }
}