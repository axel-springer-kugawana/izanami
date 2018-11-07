package domains.script
import java.util.function.BiConsumer

import akka.Done
import akka.actor.ActorSystem
import cats.effect.{Async, Effect, IO, Sync}
import domains.script.Script.ScriptCache
import domains.{AuthInfo, IsAllowed, Key}
import env.Env
import javax.script.{Invocable, ScriptEngine, ScriptEngineManager}
import play.api.Logger
import play.api.cache.AsyncCacheApi
import play.api.libs.json
import play.api.libs.json._
import play.api.libs.ws.{WSRequest, WSResponse}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

trait FeatureScript {
  def enabled(context: play.api.libs.json.JsObject,
              enabled: () => Unit,
              disabled: () => Unit,
              http: play.api.libs.ws.WSClient)(
      implicit ec: ExecutionContext
  ): Unit
}

object ScriptInstances {

  implicit val reads: Reads[Script] = Reads[Script] {
    case js: JsObject if (js \ "type").asOpt[String].contains("scala") =>
      (js \ "script")
        .asOpt[String]
        .map(s => JsSuccess(ScalaScript(s)))
        .getOrElse(JsError("missing.field.script"))
    case js: JsObject if (js \ "type").asOpt[String].contains("javascript") =>
      (js \ "script")
        .asOpt[String]
        .map(s => JsSuccess(JavascriptScript(s)))
        .getOrElse(JsError("missing.field.script"))
    case JsString(str) =>
      json.JsSuccess(JavascriptScript(str))
    case _ =>
      JsError("invalid.script")
  }

  implicit val writes: Writes[Script] = Writes[Script] {
    case JavascriptScript(script) =>
      Json.obj("type" -> "javascript", "script" -> script)
    case ScalaScript(script) =>
      Json.obj("type" -> "scala", "script" -> script)
  }

  implicit def runnableScript[F[_]: Async: ScriptCache]: RunnableScript[F, Script] = new RunnableScript[F, Script] {
    override def run(script: Script, context: JsObject, env: Env): F[Boolean] = {
      import env.scriptExecutionContext
      script match {
        case s: ScalaScript =>
          executeScalaScript[F](s, context, env)
        case s: JavascriptScript =>
          executeJavascriptScript[F](s, context, env)
      }
    }
  }

  private def executeScalaScript[F[_]: Async: ScriptCache](script: ScalaScript, context: JsObject, env: Env)(
      implicit ec: ScriptExecutionContext
  ): F[Boolean] = {
    import cats.implicits._
    import scala.collection.JavaConverters._

    val finalScript: String =
      s"""
         |import domains.script.FeatureScript
         |import play.api.libs.ws._
         |import scala.concurrent._
         |import play.api.libs.json._
         |
       |new FeatureScript {
         |  ${script.script}
         |}
      """.stripMargin

    val id = MurmurHash3.stringHash(finalScript).toString

    def buildScript: F[FeatureScript] =
      Async[F].async { cb =>
        ec.execute { () =>
          Try {
            val engineManager: ScriptEngineManager = new ScriptEngineManager
            val mayBeEngine = engineManager.getEngineFactories.asScala
            //.find(_.getEngineName === "Scala REPL")
              .find(_.getEngineName === "scala")
              .map(_.getScriptEngine)
              .map(_.asInstanceOf[ScriptEngine with Invocable])
            val engine = mayBeEngine.getOrElse(throw new IllegalArgumentException(s"Scala engine not found"))
            engine.eval(finalScript).asInstanceOf[FeatureScript]
          } recover {
            case e => cb(Left(e))
          }
        }
      }

    def run(featureScript: FeatureScript): F[Boolean] =
      Async[F].async { cb =>
        ec.execute { () =>
          Try {
            val enabled  = () => cb(Right(true))
            val disabled = () => cb(Right(false))
            featureScript.enabled(context, enabled, disabled, env.wSClient)(ec)
          } recover {
            case e => cb(Left(e))
          }
        }
      }

    val scriptCache = ScriptCache[F]

    for {
      mayBeScript <- scriptCache.get(id)
      script      <- mayBeScript.fold(buildScript)(_.pure[F])
      _           <- scriptCache.set(id, script)
      r           <- run(script)
    } yield r

  }

  private def executeJavascriptScript[F[_]: Async](script: JavascriptScript, context: JsObject, env: Env)(
      implicit ec: ScriptExecutionContext
  ): F[Boolean] = {
    val engineManager: ScriptEngineManager = new ScriptEngineManager
    val engine = engineManager
      .getEngineByName("nashorn")
      .asInstanceOf[ScriptEngine with Invocable]

    Async[F].async { cb =>
      ec.execute { () =>
        engine.eval(script.script)
        val enabled                                   = () => cb(Right(true))
        val disabled                                  = () => cb(Right(false))
        val contextMap: java.util.Map[String, AnyRef] = jsObjectToMap(context)
        Try {
          engine.invokeFunction("enabled", contextMap, enabled, disabled, new HttpClient(env, cb))
        } recover {
          case e => cb(Left(e))
        }
      }
    }
  }

  private def jsObjectToMap(jsObject: JsObject): java.util.Map[String, AnyRef] = {
    import scala.collection.JavaConverters._
    jsObject.value.mapValues(asMap).toMap.asJava
  }

  private def asMap(jsValue: JsValue): AnyRef = {
    import scala.collection.JavaConverters._
    jsValue match {
      case JsString(s)        => s
      case JsNumber(value)    => value
      case JsArray(arr)       => arr.map(v => asMap(v)).asJava
      case jsObj: JsObject    => jsObjectToMap(jsObj)
      case JsBoolean(b) if b  => java.lang.Boolean.TRUE
      case JsBoolean(b) if !b => java.lang.Boolean.FALSE
      case _                  => null
    }
  }

}

class PlayScriptCache[F[_]: Async](api: AsyncCacheApi) extends ScriptCache[F] {

  import cats._
  import cats.implicits._

  override def get(id: String): F[Option[FeatureScript]] =
    IO.fromFuture(IO(api.get[FeatureScript](id))).to[F]

  override def set(id: String, value: FeatureScript): F[Unit] = {
    val update = IO.fromFuture(IO(api.set(id, value))).to[F]
    for {
      mayBeResult <- get(id)
      _           <- mayBeResult.fold(update)(_ => Async[F].pure(Done))
    } yield ()
  }
}

case class ScriptExecutionContext(actorSystem: ActorSystem) extends ExecutionContext {
  private val executionContext: ExecutionContext =
    actorSystem.dispatchers.lookup("izanami.script-dispatcher")
  override def execute(runnable: Runnable): Unit =
    executionContext.execute(runnable)
  override def reportFailure(cause: Throwable): Unit =
    executionContext.reportFailure(cause)
}

class HttpClient[F[_]](env: Env, promise: Either[Throwable, Boolean] => Unit)(implicit ec: ScriptExecutionContext) {
  def call(optionsMap: java.util.Map[String, AnyRef], callback: BiConsumer[String, String]): Unit = {
    import play.api.libs.ws.JsonBodyWritables._

    import scala.collection.JavaConverters._
    val options: mutable.Map[String, AnyRef] = optionsMap.asScala
    val url: String                          = options("url").asInstanceOf[String]
    val method: String                       = options.getOrElse("method", "get").asInstanceOf[String]
    val headers: mutable.Map[String, String] =
      options
        .getOrElse("headers", new java.util.HashMap[String, String]())
        .asInstanceOf[java.util.Map[String, String]]
        .asScala
    val body: String =
      options.get("body").asInstanceOf[Option[String]].getOrElse("")

    val req: WSRequest =
      env.wSClient.url(url).withHttpHeaders(headers.toSeq: _*)
    val call: Future[WSResponse] = method.toLowerCase() match {
      case "get"    => req.get()
      case "post"   => req.post(body)
      case "put"    => req.put(body)
      case "delete" => req.delete()
      case "option" => req.options()
      case "patch"  => req.delete()
    }
    call.onComplete {
      case Success(response) =>
        Logger.debug(
          s"Script call $url, method=[$method], headers: $headers, body=[$body], response: code=${response.status} body=${response.body}"
        )
        Try {
          callback.accept(null, response.body)
        }.recover {
          case e => promise(Left(e))
        }
      case Failure(e) =>
        Logger.debug(s"Script call $url, method=[$method], headers: $headers, body=[$body], call failed", e)
        Try {
          callback.accept(e.getMessage, null)
        }.recover {
          case e => promise(Left(e))
        }
    }
  }
}

object GlobalScriptInstances {

  implicit val isAllowed: IsAllowed[GlobalScript] = new IsAllowed[GlobalScript] {
    override def isAllowed(value: GlobalScript)(auth: Option[AuthInfo]): Boolean = Key.isAllowed(value.id)(auth)
  }

  implicit val format = {
    import ScriptInstances._
    Json.format[GlobalScript]
  }

}
