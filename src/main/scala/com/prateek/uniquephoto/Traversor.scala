package com.prateek.uniquephoto

import scala.jdk.StreamConverters._
import scala.util.Using

import java.io.File
import java.nio.file.{ Files, Path, Paths }

object Traversor {

  type ImageDirectoryTuple = (Option[ImageDirectory], Seq[ImageDirectory])

  def traverse(root: Path): Seq[ImageDirectory] = {

    val scalaPaths: List[String] =
      Using(Files.list(root)) { s => s.toScala(List).map(_.toString) }.get

    val childPaths: ImageDirectoryTuple =
      scalaPaths.foldLeft(ImageDirectoryTuple)((idt, path) => {
        path match {
          case ExcludeDirectory() => idt
          case Directory() =>
            val childImageDirectories = traverse(Paths.get(path))
            val updatedImageDirectories =
              idt._2.appendedAll(childImageDirectories)
            (idt._1, updatedImageDirectories)
          case ImageFile() =>
            val newIdt = idt._1
              .map(id => {
                val updatedFiles = id.files.prepended(path)
                id.copy(files = updatedFiles)
              })
              .orElse(
                Some(ImageDirectory(root, Seq(path)))
              )
            (newIdt, idt._2)
          case _ =>
//            println(s"excluding: $path")
            idt
        }
      })

    val ret = childPaths._1
      .map(id => childPaths._2.prepended(id))
      .getOrElse(childPaths._2)
//    println(s"ImageDirectories parsed for root: $root $ret")
    ret
  }

  // refer to https://alvinalexander.com/scala/scala-type-examples-type-aliases-members/
  def ImageDirectoryTuple: ImageDirectoryTuple = (None, Seq.empty)

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
        "ProgramFiles"
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
}

case class ImageDirectory(dir: Path, files: Seq[String]) {
  override def toString: String = {
    s"dir: $dir, \r\nfile: $files\r\n"
  }
}
