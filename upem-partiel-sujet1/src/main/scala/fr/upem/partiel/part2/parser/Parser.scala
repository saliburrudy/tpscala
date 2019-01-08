package fr.upem.partiel.part2.parser

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Parser {

  // TODO
  def toDirector: String => Option[Director] = s =>{
    s match {
      case s if((s.filter(c => c.isDigit)).length > 1) => None
      case _ => Some(director(s.split("")(0), s.split("")(1)))
    }

  }

  // TODO
  def toName: String => Title = t => {
    title(t)
  }

  // TODO
  def toCountry: String => Option[Country] = c => {
    c match {
      case c if (c == "FR") => Some(Country.France)
      case c if (c == "UK") => Some(Country.England)
      case c if (c == "IT") => Some(Country.Italy)
      case c if (c == "GE") => Some(Country.Germany)
      case c if (c == "US") => Some(Country.UnitedStates)
      case _ => None
    }
  }

  // TODO
  def toYear: String => Option[Year] = y => {
    y match {
      case y if(!y.forall(_.isDigit)) =>{println("pas que des digits : "+y); None}
      case y if(y.length > 4 || y.length < 4) => {println("trop grand ou trop petite : "+y);None}
      case y if(y.toInt >= 3000) => {println("pas le bon numéro au début : "+y); None}
      case _ =>  {println("C'est bon : "+y);Some(year(y.toInt))}
    }
  }

  // TODO
  def toViews: BigDecimal => Option[Views] = bd => {
    bd match {
      case bd if(bd < 0) => None
      case _ => Some(views(bd.toLongExact))
    }
  }

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
