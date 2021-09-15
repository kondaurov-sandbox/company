package task

import org.json4s.JsonAST.JValue
import org.json4s.DefaultFormats

object Model {

  implicit val formats: DefaultFormats = DefaultFormats

  object Company {
    def fromCsv(cols: Array[String]): Company = {
      Company(
        name = cols(0).replace("\"", ""),
        revenue = cols(1).toInt,
        isProfitable = cols(2) == "true"
      )
    }
  }

  case class Company(
    name: String,
    revenue: Int,
    isProfitable: Boolean
  )

  object Filter {

    def fromJson(params: JValue): Filter = {
      (params \ "filterType").extract[String] match {
        case "NameContains" => NameContains(text = (params \ "text").extract[String])
        case "IsProfitable" => IsProfitable
        case f => throw new Error(s"Not supported filter '$f'")
      }
    }

  }

  sealed trait Filter {
    def check(company: Company): Boolean
  }

  case class NameContains(
    text: String
  ) extends Filter {
    def check(company: Company): Boolean = company.name.contains(text)
  }

  case object IsProfitable extends Filter {
    def check(company: Company): Boolean = company.isProfitable
  }

}
