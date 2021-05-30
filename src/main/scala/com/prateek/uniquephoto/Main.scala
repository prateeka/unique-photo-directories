package com.prateek.uniquephoto

import java.nio.file.Paths

object Main extends App {
  Traversor
    .traverse(Paths.get("/Users/prateek/code/pattu/unique-photo-directories"))
    .foreach(println)
}
