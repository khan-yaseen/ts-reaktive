package com.tradeshift.reaktive.xsd

import akka.stream.scaladsl.StreamConverters
import com.tradeshift.reaktive.testkit.SharedActorSystemSpec
import java.io.FileInputStream
import org.scalatest.Matchers
import org.scalatest.FunSpecLike
import akka.stream.scaladsl.StreamConverters
import akka.stream.scaladsl.FileIO
import com.tradeshift.reaktive.marshal.stream.AaltoReader
import com.tradeshift.reaktive.xsd.SchemaItem.Import
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import org.scalatest.AsyncFunSpecLike
import scala.concurrent.Await
import scala.concurrent.duration._

class SchemaLoaderSpec extends SharedActorSystemSpec with FunSpecLike with Matchers {
  implicit val m = materializer
  val basePath = "./src/main/resources/"
  var stuff = Vector.empty[Schema]
  
  describe("SchemaLoader") {
    it("should load a UBL XSD correctly") {
      val start = System.nanoTime()
      val schema = Await.result(SchemaLoader("urn:oasis:names:specification:ubl:schema:xsd:Invoice-2",
        i => i match {
          case Import("urn:oasis:names:specification:ubl:schema:xsd:Invoice-2", _) =>
            Import(
              "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2",
              Some("UBL-Invoice-2.2-annotated.xsd"))
          case Import("http://www.w3.org/2000/09/xmldsig#", _) =>
            Import(
              "http://www.w3.org/2000/09/xmldsig#",
              Some("ubl/common/UBL-xmldsig-core-schema-2.2.xsd"))
          case Import(ns, Some(file)) =>
            val i = file.lastIndexOf("/")
            val f = if (i == -1) file else file.substring(i + 1)
            Import(ns, Some("ubl/common/" + f))
        },
        i => i match {
          case Import(ns, Some(file)) =>
            println("Importing " + ns + " from " + file)
            StreamConverters.fromInputStream(() => getClass.getResourceAsStream("/" + file))
              .via(AaltoReader.instance)
        }), 10.seconds)

        val t = System.nanoTime() - start
        println(s"Took ${(t / 1000000)}ms")
        stuff = stuff :+ schema
        schema.namespaces should have size(14)
    }
  }
}
