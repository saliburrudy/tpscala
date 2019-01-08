package fr.upem.partiel.part2.functions

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Director

object Functions {

  // TODO
  lazy val getDirectorNames: List[Movie] => List[String] = {
    case x :: xs => x.title :: getDirectorNames(xs)
    case Nil => List()
  }

  // TODO
  lazy val viewMoreThan: Long => List[Movie] => List[Movie] = ???

  // TODO
  lazy val byDirector: List[Movie] => Map[Director, List[Movie]] = ???

}
