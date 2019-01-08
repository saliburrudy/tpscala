package fr.upem.partiel.part2.functions

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Director

object Functions {
  // TODO
  lazy val getDirectorNames: List[Movie] => List[String] = l => {
    l.map(movie => s"${movie.director.firstName} ${movie.director.lastName}")
  }

  // TODO
  lazy val viewMoreThan: Long => List[Movie] => List[Movie] = long => list => {
    list.filter(_.views.views > long)
  }

  // TODO
  lazy val byDirector: List[Movie] => Map[Director, List[Movie]] = list => {
    list.groupBy(_.director)
  }

}
