package com.prateek.uniquephoto

import scala.jdk.StreamConverters._

import java.nio.file.{ Files, Path }

object Traversor {

  type ImageDirectoryTuple = (Option[ImageDirectory], Seq[ImageDirectory])

  def traverse(root: Path): Seq[ImageDirectory] = {
    val scalaPaths: LazyList[Path] = Files.list(root).toScala(LazyList)
    val childPaths: ImageDirectoryTuple =
      scalaPaths.foldLeft(ImageDirectoryTuple)((idt, path) => {
        path match {
          case Directory() =>
            val childImageDirectories = traverse(path)
            val updatedImageDirectories =
              idt._2.appendedAll(childImageDirectories)
            (idt._1, updatedImageDirectories)
          case ImageFile() =>
            val newIdt = idt._1
              .map(id => {
                val updatedFiles = id.files.prepended(path.getFileName.toString)
                id.copy(files = updatedFiles)
              })
              .orElse(
                Some(ImageDirectory(root, Seq(path.getFileName.toString)))
              )
            (newIdt, idt._2)
          case _ => idt
        }
      })
    childPaths._1
      .map(id => childPaths._2.prepended(id))
      .getOrElse(childPaths._2)
  }

  // refer to https://alvinalexander.com/scala/scala-type-examples-type-aliases-members/
  def ImageDirectoryTuple: ImageDirectoryTuple = (None, Seq.empty)

  object Directory {
    def unapply(path: Path): Boolean = path.toFile.isDirectory
  }

  object ImageFile {
    import org.apache.tika.Tika
    val tika = new Tika

    def unapply(path: Path): Boolean = {
      val bool = path.toString.matches("(?i).+\\.(jpg|avi)$")
//      println(s"file: ${path.getFileName}, bool: $bool")
      bool
    }
  }
}

case class ImageDirectory(dir: Path, files: Seq[String]) {
  override def toString: String = {
    s"dir: $dir, \r\nfile: ${files.foreach(println)}\r\n"
  }
}
