package fr.upem.partiel.part2.model

import fr.upem.partiel.part2.model.Movie._

// TODO You have to create all the classes you need for the exam
// TODO Don't forget to read the existing code and the unit tests to get some clues !

// TODO Create this model
trait Movie {
  val title:Title
  val director:Director
  val year:Year
  val views:Views
  val country:Country
}

object Movie {

  def apply(title: Title, director: Director, year: Year, views: Views, country: Country): Movie = {
    new Movie {
      override val title: Title = title
      override val director: Director = director
      override val year: Year = year
      override val views: Views = views
      override val country: Country = country
    }
  }

  // TODO Create this model
  trait Title {
    val title: String
  }

  // TODO Create this model
  trait Director {
    val firstName: String
    val lastName: String
  }

  // TODO Create this model
  trait Year {
    val year: Int
  }

  // TODO Create this model
  trait Views {
    val views: Long
  }

  trait Country

  object Country {

    final case object France extends Country

    final case object England extends Country

    final case object Italy extends Country

    final case object Germany extends Country

    final case object UnitedStates extends Country

  }

  // TODO Create this method
  def movie(title: Title, director: Director, year: Year, views: Views, country: Country): Movie = {
    Movie(title, director, year, views, country)
  }

  // TODO Create this method
  def title(s: String): Title = {
    new Title {
      override val title: String = s
    }
  }

  // TODO Create this method
  def director(fn: String, ln: String): Director = {
    new Director {
      override val firstName: String = fn
      override val lastName: String = ln
    }
  }

  // TODO Create this method
  def year(value: Int): Year = {
    new Year {
      override val year: Int = value
    }
  }

  // TODO Create this method
  def views(value: Long): Views = {
    new Views {
      override val views: Long = value
    }
  }

}