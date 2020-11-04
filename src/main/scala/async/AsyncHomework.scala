package async

import java.net.URL
import java.util.concurrent.Executors
import scala.util.{Failure, Success}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  if (args.isEmpty)
    process("http://google.com/")
  else
    args.foreach(process)

  def process(url: String): Unit = {
    val body = fetchPageBody(url)
    body.onComplete {
      case Success(value)     => printServersNames(value)
      case Failure(exception) => exception.printStackTrace()
    }
  }

  private def printServersNames(body: String): Unit = {
    val linkUrls = findLinkUrls(body)
    linkUrls.onComplete {
      case Success(value) =>
        val seq = Future.sequence(value.map(x => fetchServerName(x)))
        seq.onComplete {
          case Success(value) => {
            value.map { x =>
              x.getOrElse("Failed to fetch server name")
            }
          }.sortBy(_.toUpperCase).foreach(println)
          case Failure(exception) => exception.printStackTrace()
        }
      case Failure(exception) => exception.printStackTrace()
    }
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
