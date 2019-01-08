package fr.upem.partiel.part2.parser

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Parser {

  // TODO
  def toDirector: String => Option[Director] = ???

  // TODO
  def toName: String => Title = ???

  // TODO
  def toCountry: String => Option[Country] = ???

  // TODO
  def toYear: String => Option[Year] = ???

  // TODO
  def toViews: BigDecimal => Option[Views] = ???

  implicit val directorReads = Reads[Director] {
    case JsString(value) => toDirector(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Director"))
    case _ => JsError("Not a valid type for Director")
  }

  implicit val nameReads = Reads[Title] {
    case JsString(value) => JsSuccess(toName(value))
    case _ => JsError("Not a valid type for Name")
  }

  implicit val countryReads = Reads[Country] {
    case JsString(value) => toCountry(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Country"))
    case _ => JsError("Not a valid type for Country")
  }

  implicit val yearReads = Reads[Year] {
    case JsString(value) => toYear(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Year"))
    case _ => JsError("Not a valid type for Year")
  }

  implicit val viewsReads = Reads[Views] {
    case JsNumber(value) => toViews(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Views"))
    case _ => JsError("Not a valid type for Views")
  }

  implicit val movieReads: Reads[Movie] = (
    (__ \ "title").read[Title] and
      (__ \ "director").read[Director] and
      (__ \ "year").read[Year] and
      (__ \ "views").read[Views] and
      (__ \ "country").read[Country]
    ) (Movie.apply _)

}
