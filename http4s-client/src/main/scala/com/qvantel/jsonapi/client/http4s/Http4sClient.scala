package com.qvantel.jsonapi.client.http4s

import cats.effect._
import cats.instances.list._
import cats.syntax.traverse._
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.{Uri => CoreUri}
import com.netaporter.uri.dsl._
import org.http4s.Status.Successful
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.io._

import com.qvantel.jsonapi._
import com.qvantel.jsonapi.client.http4s.JsonApiInstances._

trait Http4sClient extends Http4sClientDsl[IO] {
  private[this] def mkIncludeString(include: Set[String]): Option[String] =
    if (include.isEmpty) {
      None
    } else {
      Some(include.mkString(","))
    }

  implicit def instance(implicit endpoint: ApiEndpoint, client: Client[IO]): JsonApiClient = new JsonApiClient {

    implicit val uConfig: UriConfig = uriConfig

    override def one[A](id: String, include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                                      reader: JsonApiReader[A]): IO[Option[A]] =
      for {
        config <- endpoint.config
        response <- pathOne(config.uri / pt.self(id), include)
      } yield response

    override def many[A](ids: Set[String], include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                                             reader: JsonApiReader[A]): IO[List[A]] =
      ids.toList.traverse { id =>
        one(id, include).flatMap {
          case Some(entity) => IO.pure(entity)
          case None         => IO.raiseError(ApiError.NoEntityForId(id, pt.root))
        }
      }

    override def filter[A](filter: String, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                             reader: JsonApiReader[A]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri / pt.root ? ("filter" -> filter) ? ("include" -> mkIncludeString(include))))
        response <- client.expect[List[A]](uri)
      } yield response
    }

    override def pathOne[A](path: CoreUri, include: Set[String] = Set.empty)(
        implicit reader: JsonApiReader[A]): IO[Option[A]] = {
      implicit val _include: Include = Include(include)

      val request = for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri.copy(pathParts = path.pathParts) ? ("include" -> mkIncludeString(include))))
        request <- GET(uri)
      } yield request

      client.fetch(request) {
        case Successful(resp) => resp.as[A].map(Some(_))
        case NotFound(_)      => IO.pure(None)
        case failedResponse   => IO.raiseError(UnexpectedStatus(failedResponse.status))
      }
    }

    override def pathMany[A](path: CoreUri, include: Set[String] = Set.empty)(
        implicit reader: JsonApiReader[A]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri.copy(pathParts = path.pathParts) ? ("include" -> mkIncludeString(include))))
        response <- client.expect[List[A]](uri)
      } yield response
    }

    override def post[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      val request = for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri / pt.entity(entity)))
        req <- POST(uri, entity)
      } yield req

      client.fetch(request) {
        case Successful(resp) => resp.as[Response]
        case failedResponse   => IO.raiseError(UnexpectedStatus(failedResponse.status))
      }
    }

    override def put[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri / pt.entity(entity)))
        req      <- PUT(uri, entity)
        response <- client.fetchAs[Response](req)
      } yield response
    }

    override def patch[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri / pt.entity(entity)))
        req      <- PATCH(uri, entity)
        response <- client.fetchAs[Response](req)
      } yield response
    }

    override def delete[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString(config.uri / pt.entity(entity)))
        req      <- DELETE(uri)
        response <- client.fetchAs[Response](req)
      } yield response
    }
  }
}

object Http4sClient extends Http4sClient
