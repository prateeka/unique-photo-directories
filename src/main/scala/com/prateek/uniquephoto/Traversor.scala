package com.prateek.uniquephoto

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.Using

import java.io.File
import java.nio.file.{ Files, Path, Paths }
import java.util.concurrent._

case class ImageDirectory(dir: Path, files: Seq[String]) {
  override def toString: String = {
    s"dir: $dir, \r\nfile: $files\r\n"
  }
}

class Traversor {

  private type ImageDirectoryOption = Option[ImageDirectory]
  private val allImageDirectories: ListBuffer[ImageDirectory] =
    new ListBuffer[ImageDirectory]()
  private val pool: ExecutorService =
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors - 2)
//    Executors.newFixedThreadPool(1)
  private val clq = new ConcurrentLinkedQueue[Future[ImageDirectoryOption]]()

  def start(root: Path): ListBuffer[ImageDirectory] = {
    traverse(root)
    while (!clq.isEmpty) {
      val f = clq.poll()
      f.get.foreach(id => {
        println(s"appending $id")
        allImageDirectories.append(id)
      })
    }
    pool.shutdown()
    allImageDirectories
  }

  def traverse(root: Path): Unit = {
    val f = pool.submit(new DirectoryExaminer(root))
    clq.add(f)
  }

  class DirectoryExaminer(dir: Path) extends Callable[ImageDirectoryOption] {
    override def call(): Option[ImageDirectory] = {
      val scalaPaths: List[String] =
        Using(Files.list(dir)) { s => s.toScala(List).map(_.toString) }.get

      val childPaths: mutable.Seq[String] =
        scalaPaths.foldLeft(new ListBuffer[String])((acc, path) => {
          path match {
            case ExcludeDirectory() => acc
            case Directory() =>
              traverse(Paths.get(path))
              acc
            case ImageFile() =>
              acc.prepend(path)
            case _ =>
              //            println(s"excluding: $path")
              acc
          }
        })

      var ret: ImageDirectory = null
      if (childPaths.nonEmpty) {
        ret = ImageDirectory(dir, childPaths.reverse.toList)
      }
      Option(ret)
    }
  }
}

object ExcludeDirectory {
  val excludeDirectories: Set[Path] = Set(
    "/usr/sbin/authserver",
    "/usr/lib/cron",
    "/etc/cups/certs",
    "/var",
    "/Library",
    "/Users/prateek/Library",
    "/Users/Shared",
    "/Users/prateek/.Trash"
  ).map(Paths.get(_))

  val endsWith: Set[String] =
    Set(
      "docs",
      "entertainment",
      "sangeet",
      "eclipse-jee-galileo-SR2-win32",
      "books",
      "ProgramFiles",
      "Old Songs",
      "Kazaa",
      "Mahabharat",
      "Iskcon",
      "eclipse-cpp-galileo-SR1-win32",
      "eclipse_workspace",
      "mailInRebate",
      "automobiles",
      "krishna",
      "Software",
      "userSoftware",
      "hi",
      "Ghazals",
      "humtum",
      "workspace",
      "Songs New"
    )

  def unapply(strPath: String): Boolean = {
    val path = Paths.get(strPath)

    excludeDirectories.contains(path) ||
    endsWith.exists(e => path.endsWith(e))
  }
}

object Directory {
  def unapply(path: String): Boolean = new File(path).isDirectory
}

object ImageFile {
  def unapply(path: String): Boolean = {
    val bool = path.matches("(?i).+\\.(jpg|avi)$")
//      println(s"file: ${path.getFileName}, bool: $bool")
    bool
  }
}
