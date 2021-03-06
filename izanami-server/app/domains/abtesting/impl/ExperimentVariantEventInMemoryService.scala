package domains.abtesting.impl

import akka.{Done, NotUsed}
import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.scaladsl.Source
import domains.abtesting._
import env.DbDomainConfig
import store.Result.Result
import store.Result
import ExperimentDataStoreActor._
import cats.effect.Effect
import domains.abtesting.ExperimentVariantEvent.eventAggregation
import domains.events.EventStore
import domains.events.Events.ExperimentVariantEventCreated

//////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////    IN MEMORY     ////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

object ExperimentVariantEventInMemoryService {
  def apply[F[_]: Effect](
      configdb: DbDomainConfig,
      eventStore: EventStore[F]
  )(implicit actorSystem: ActorSystem): ExperimentVariantEventInMemoryService[F] =
    new ExperimentVariantEventInMemoryService(configdb.conf.namespace, eventStore)
}

class ExperimentVariantEventInMemoryService[F[_]: Effect](namespace: String, eventStore: EventStore[F])(
    implicit actorSystem: ActorSystem
) extends ExperimentVariantEventService[F] {

  import actorSystem.dispatcher
  import akka.pattern._
  import akka.util.Timeout
  import cats.implicits._
  import cats.effect.implicits._
  import libs.effects._

  import scala.concurrent.duration.DurationInt
  import ExperimentVariantEventInstances._

  private implicit val timeout: Timeout = Timeout(5.second)

  private val store = actorSystem.actorOf(Props[ExperimentDataStoreActor](new ExperimentDataStoreActor()),
                                          namespace + "_in_memory_exp_event")

  override def create(id: ExperimentVariantEventKey, data: ExperimentVariantEvent): F[Result[ExperimentVariantEvent]] =
    (store ? AddEvent(id.experimentId.key, id.variantId, data))
      .mapTo[ExperimentVariantEvent]
      .toF
      .map[Result[ExperimentVariantEvent]](res => Result.ok(res)) <* eventStore.publish(
      ExperimentVariantEventCreated(id, data)
    )

  override def deleteEventsForExperiment(experiment: Experiment): F[Result[Done]] =
    (store ? DeleteEvents(experiment.id.key))
      .mapTo[Done]
      .toF
      .map[Result[Done]](res => Result.ok(res))

  override def findVariantResult(experiment: Experiment): Source[VariantResult, NotUsed] =
    Source
      .fromFuture(
        experiment.variants.toList
          .traverse { variant =>
            findEvents(experiment, variant)
          }
          .toIO
          .unsafeToFuture()
      )
      .mapConcat(_.flatten)
      .via(eventAggregation(experiment))

  private def findEvents(experiment: Experiment, variant: Variant): F[List[ExperimentVariantEvent]] =
    (store ? FindEvents(experiment.id.key, variant.id)).mapTo[List[ExperimentVariantEvent]].toF

  override def listAll(patterns: Seq[String]): Source[ExperimentVariantEvent, NotUsed] =
    Source
      .fromFuture((store ? GetAll(patterns)).mapTo[Seq[ExperimentVariantEvent]])
      .mapConcat(_.toList)

  override def check(): F[Unit] = ().pure[F]
}

private[abtesting] class ExperimentDataStoreActor extends Actor {

  private var datas: Map[String, List[ExperimentVariantEvent]] =
    Map.empty[String, List[ExperimentVariantEvent]]

  val experimentseventsNamespace: String = "experimentsevents"

  def transformation(displayed: Long, won: Long): Double =
    if (displayed != 0) {
      won * 100.0 / displayed
    } else 0.0

  override def receive: Receive = {
    case AddEvent(experimentId, variantId, event) =>
      val eventKey: String =
        s"$experimentseventsNamespace:$experimentId:$variantId"

      val events: List[ExperimentVariantEvent] =
        datas.getOrElse(eventKey, List.empty[ExperimentVariantEvent])

      datas = datas + (eventKey -> (event :: events))
      sender() ! event

    case FindEvents(experimentId, variantId) =>
      val eventKey: String =
        s"$experimentseventsNamespace:$experimentId:$variantId"
      sender() ! datas
        .getOrElse(eventKey, List.empty[ExperimentVariantEvent])
        .sortWith((e1, e2) => e1.date.isBefore(e2.date))

    case GetAll(patterns) =>
      sender() ! datas.values.flatten.filter(e => e.id.key.matchPatterns(patterns: _*))

    case DeleteEvents(experimentId) =>
      val eventKey: String = s"$experimentseventsNamespace:$experimentId:"

      datas.keys
        .filter(key => key.startsWith(eventKey))
        .foreach(key => datas = datas - key)

      sender() ! Done

    case m =>
      unhandled(m)
  }
}

private[abtesting] object ExperimentDataStoreActor {

  sealed trait ExperimentDataMessages

  case class AddEvent(experimentId: String, variantId: String, event: ExperimentVariantEvent)
      extends ExperimentDataMessages

  case class FindEvents(experimentId: String, variantId: String) extends ExperimentDataMessages

  case class GetAll(patterns: Seq[String]) extends ExperimentDataMessages

  case class DeleteEvents(experimentId: String) extends ExperimentDataMessages

}
