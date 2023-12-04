package utils

import java.io.{BufferedReader, FileReader}
import scala.util.Using

class InputFileReader

object InputFileReader:
  def getLinesFromFile(pathToFile: String): Seq[String] =
    Using.resource(new BufferedReader(new FileReader(pathToFile))) { reader =>
      Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
    }
