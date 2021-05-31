package com.prateek.uniquephoto

import java.nio.file.Paths

object Main extends App {
  Traversor
//    .traverse(Paths.get("/"))
    //    .traverse(Paths.get("/Users"))
    .traverse(Paths.get("/Volumes/FreeAgent Drive/backup"))
    .foreach(println)
}
