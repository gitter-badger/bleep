package bleep
package commands

import bleep.internal.{asYamlString, CoursierLogger, FileUtils, ScalaVersions}
import coursier.Repository
import coursier.cache.FileCache
import coursier.core.{Dependency, Versions}
import coursier.util.Task

import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

case class BuildUpdateDeps(started: Started) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    // ensure uniqueness of coursier deps so we only fetch each once
    val byCoursierDep: Map[Dependency, Dep] =
      started.build.projects.flatMap { case (crossName, p) =>
        ScalaVersions.fromExplodedProject(p) match {
          case Left(err) =>
            throw new BuildException.Text(crossName, err)
          case Right(scalaVersions) =>
            p.dependencies.values.toList.map(dep => (dep.dependencyForce(crossName, scalaVersions), dep))
        }
      }

    // use bleep dependency going forward. a bleep dependency may be instantiated into multiple coursier dependencies after
    // plugging in scala version and platform
    val byBleepDep: Map[Dep, immutable.Iterable[Dependency]] =
      byCoursierDep.groupMap { case (_, bleepDep) => bleepDep } { case (csDep, _) => csDep }

    val config = started.lazyConfig.forceGet
    val repos = CoursierResolver.coursierRepos(started.rawBuild.resolvers.values, config.authentications).filter(_.repr.contains("http"))
    val fileCache = FileCache[Task]().withLogger(new CoursierLogger(started.logger))

    val foundByDep: Map[Dep, List[Versions]] = {
      implicit val ec: ExecutionContext = started.executionContext
      Await.result(fetchAllVersions(fileCache, repos, byBleepDep), Duration.Inf)
    }

    val upgrades: Map[Dep, Dep] =
      foundByDep.flatMap { case (bleepDep, versions) =>
        // one particular scala/platform combination may be dropped. let's say it'll hold the rest back for now, it's the easiest.
        val latest = versions.map(_.latest).min
        if (latest == bleepDep.version) None
        else Some(bleepDep -> bleepDep.withVersion(latest))
      }

    val newBuild = upgradedBuild(upgrades, started.rawBuild)

    FileUtils.writeString(
      started.buildPaths.bleepYamlFile,
      asYamlString(newBuild)
    )

    Right(())
  }

  def upgradedBuild(upgrades: Map[Dep, Dep], build: model.Build): model.Build = {
    def go(p: model.Project): model.Project =
      p.copy(
        cross = JsonMap(p.cross.value.map { case (crossId, p) => (crossId, go(p)) }),
        dependencies = p.dependencies.map(dep => upgrades.getOrElse(dep, dep))
      )

    build.copy(
      projects = JsonMap(build.projects.value.map { case (projectName, p) => (projectName, go(p)) }),
      templates = JsonMap(build.templates.value.map { case (templateId, p) => (templateId, go(p)) })
    )
  }

  def fetchAllVersions(fileCache: FileCache[Task], repos: List[Repository], byBleepDep: Map[Dep, immutable.Iterable[Dependency]])(implicit
      ec: ExecutionContext
  ): Future[Map[Dep, List[Versions]]] =
    Future
      .sequence {
        byBleepDep.map { case (bleepDep, csDeps) =>
          Future.sequence(csDeps.map(csDep => fetchVersions(fileCache, repos, csDep))).map(ofs => (bleepDep, ofs))
        }
      }
      .map(_.collect { case (dep, maybeFounds) => (dep, maybeFounds.flatten.toList) }.toMap)

  def fetchVersions(fileCache: FileCache[Task], repos: List[Repository], csDep: Dependency)(implicit ec: ExecutionContext): Future[Option[Versions]] =
    repos match {
      case repo :: restRepos =>
        repo.versions(csDep.module, fileCache.fetch, versionsCheckHasModule = true).run.future().transformWith {
          case Success(Right((versions, _))) => Future.successful(Some(versions))
          case _                             => fetchVersions(fileCache, restRepos, csDep)
        }
      case Nil => Future.successful(None)
    }
}
