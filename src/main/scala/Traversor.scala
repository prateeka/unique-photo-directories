package com.prateek.uniquephoto

import scala.jdk.StreamConverters._

import java.nio.file.{ Files, Path }

object Traversor {

  type ImageDirectoryTuple = (Option[ImageDirectory], Seq[ImageDirectory])

  def traverse(root: Path): Seq[ImageDirectory] = {
    val scalaPaths: LazyList[Path] = Files.list(root).toScala(LazyList)
    val childPaths: ImageDirectoryTuple =
      scalaPaths.foldLeft(ImageDirectoryTuple)((idt, path) => {
        path.toFile.isDirectory match {
          case true                                                                      =>
            val childImageDirectories = traverse(path)
            val updatedImageDirectories =
              idt._2.appendedAll(childImageDirectories)
            (idt._1, updatedImageDirectories)
          case false if Option(Files.probeContentType(path)).exists(_.contains("pdf")) =>
            val newIdt = idt._1
              .map(id => {
                val updatedFiles = id.files.prepended(path)
                id.copy(files = updatedFiles)
              })
              .orElse(Some(ImageDirectory(root, Seq(path))))
            (newIdt, idt._2)
          case _                                                                         => idt
        }
      })
    childPaths._1
      .map(id => childPaths._2.prepended(id))
      .getOrElse(childPaths._2)
  }

  // refer to https://alvinalexander.com/scala/scala-type-examples-type-aliases-members/
  def ImageDirectoryTuple: ImageDirectoryTuple = (None, Seq.empty)
}
case class ImageDirectory(dir: Path, files: Seq[Path]) {
  override def toString: String = {
    s"dir: $dir, \r\nfile: ${files.map(f => f.getFileName)}\r\n"
  }
}
