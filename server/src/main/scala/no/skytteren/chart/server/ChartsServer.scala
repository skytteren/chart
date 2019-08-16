package no.skytteren.chart.server

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import no.skytteren.chart.Charts

import scala.collection.mutable
import scala.concurrent.Promise

object ChartsServer{

  type Name = String
  val config = ConfigFactory.load()

  implicit val system = ActorSystem("chart-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  val registeredCharts = mutable.Map[Name, Charts.Bundle#Frag]()

  def push(chart: Charts.Bundle#Frag, name: String = UUID.randomUUID().toString): Unit = {
    registeredCharts.put(name, chart)
    println(s"Added: http://0.0.0.0:4321/$name or for html http://0.0.0.0:4321/html/$name ")
  }

  def chartsRoute: Route = {
    path("html" / Segment){ name =>
      registeredCharts.get(name).map( chart =>
        complete(HttpResponse(
          entity = HttpEntity(ContentTypes.`text/html(UTF-8)`,
            ChartView.chart(name, registeredCharts(name)).render
          )
        ))
      ).getOrElse(complete(StatusCodes.NotFound))
    } ~ path(Segment){ name =>
      registeredCharts.get(name).map( chart =>
        complete(HttpResponse(
          entity = HttpEntity(
            MediaTypes.`image/svgz`.toContentType,
            Gzip.newCompressor.compress(ByteString(registeredCharts(name).render))
          )
        ))
      ).getOrElse(complete(StatusCodes.NotFound))
    } ~ pathEndOrSingleSlash{
      complete(HttpResponse(
        entity = HttpEntity(ContentTypes.`text/html(UTF-8)`,
          ChartView.list(registeredCharts.keys.toSeq).render
        )
      ))
    }
  }

  val indexRoute: Route = {
      concat(
        path("examples") {
          complete(HttpResponse(
            entity = HttpEntity(ContentTypes.`text/html(UTF-8)`,
              ChartView.examples.render
            )
          ))
        },
        chartsRoute
      )
    }

  val binding: Promise[ServerBinding] = Promise()
  binding.completeWith(
    Http().bindAndHandle(
      indexRoute,
      "0.0.0.0",
      4321
    )
  )


  def start(): Unit = {
    binding.future.foreach {
      sb: ServerBinding =>
        println(s"""Server online at http://localhost:${sb.localAddress.getPort}/chart""")
    }
  }

  def stop(): Unit = {
    binding.future.foreach { sb =>
            sb.unbind() // trigger unbinding from the port
              .onComplete(_ => system.terminate()) // and shutdown when done
    }
  }

  def main(strings: Array[String]): Unit = {
    start()
    binding.future.foreach { sb =>
      Option(System.console).foreach {
        console =>
          console.readLine("Press ENTER to stop server")

          sb.unbind() // trigger unbinding from the port
            .onComplete(_ => system.terminate()) // and shutdown when done
      }
    }
  }


}
