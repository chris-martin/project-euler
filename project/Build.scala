import sbt._
import Keys._
import scala.collection.immutable

object Build extends sbt.Build {

  override def projectDefinitions(baseDirectory: File): Seq[Project] = {

    val modules = immutable.Seq((
      for (x <- file(".").listFiles if x.isDirectory && x.getName.matches("[0-9]+")) yield {
        Project(id = "problem-%s".format(x.getName), base = x)
      }
    ): _*)

    modules :+ Project(id = "root", base = file("."), aggregate = modules.map(x => x:ProjectReference))
  }

}
