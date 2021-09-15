package task

import org.json4s.jackson.JsonMethods
import task.Model.{Company, Filter}

import java.io.File
import scala.io.Source
import scala.util.{Try, Using}

object Main extends App {

  val companies = readCompanies("runtime/companies.csv").get
  val filters = readFilters("runtime/filters.json").get

  val result = companies
    .filterNot(c => filters.exists(f => !f.check(c)))
    .sortBy(c => c.revenue)
    .reverse
    .take(3)

  println("Top companies:")
  result.foreach(c => println(c))

  def readCompanies(
    inputCsvFile: String
  ): Try[Seq[Company]] = {

    for {
      input <- Using(Source.fromFile(inputCsvFile))(_.getLines().drop(1).toSeq)
      result <- Try(input.map(line => Company.fromCsv(line.split(','))))
    } yield result

  }

  def readFilters(
    inputJsonFile: String
  ): Try[Seq[Filter]] = {
    for {
      input <- Try(JsonMethods.parse(new File(inputJsonFile)))
      result <- Try(input.children.map(Filter.fromJson))
    } yield result
  }

}
