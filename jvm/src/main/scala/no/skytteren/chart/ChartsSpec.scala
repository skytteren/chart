package no.skytteren.chart

import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future

object ChartsServer extends App{

  var data = List(
    {"Locke"    ->  4},
    {"Reyes"    ->  8},
    {"Ford"     -> 15},
    {"Jarrah"   -> 16},
    {"Shephard" -> 23},
    {"Kwon"     -> 42}
  )

  println(Charts.barVertical(data, 500, 400).render)
  println(Charts.barHorizontal(data, 500, 400).render)

  val config = ConfigFactory.load()

  implicit val system = ActorSystem("chart-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val indexRoute: Route = {
      concat(
        path("chart") {
          complete(HttpResponse(
            entity = HttpEntity(ContentTypes.`text/html(UTF-8)`,
              ChartView.chart.render
            )
          ))
        }
      )
    }

  val bindingFuture: Future[ServerBinding] =
    Http().bindAndHandle(
      indexRoute,
      "0.0.0.0",
      4321
    )

  bindingFuture.foreach {
    (sb: ServerBinding) ⇒
      println(s"Server online at http://${sb.localAddress.getHostString}:${sb.localAddress.getPort}")

      Option(System.console).foreach {
        console ⇒
          console.readLine("Press ENTER to stop server")

          sb.unbind()                           // trigger unbinding from the port
            .onComplete(_ ⇒ system.terminate()) // and shutdown when done
      }
  }

  println("Started!!")

}
