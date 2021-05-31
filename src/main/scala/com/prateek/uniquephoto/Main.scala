package com.prateek.uniquephoto

import java.nio.file.Paths

object Main extends App {
  val t = new Traversor
  val path =
//    (Paths.get("/"))
//    Paths.get("/Users")
    Paths.get("/Volumes/FreeAgent Drive/backup")
  t.start(path).foreach(println)
}
