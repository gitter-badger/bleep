package bleep

import coursier.cache.{ArchiveCache, CacheLogger, FileCache}
import coursier.jvm.JvmIndex
import coursier.util.{Artifact, Task}

import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class FetchNode(logger: CacheLogger, ec: ExecutionContext) {
  def apply(nodeVersion: String): Path = {
    val os = JvmIndex.defaultOs()
    val architecture = os match {
      case "darwin" =>
        import scala.sys.process._
        val uname = Seq("uname", "-a").!!.toLowerCase
        if (uname.contains("arm64")) "arm64" else JvmIndex.defaultArchitecture()
      case _ => JvmIndex.defaultArchitecture()
    }
    apply(nodeVersion, os, architecture)
  }

  def apply(nodeVersion: String, os: String, architecture: String): Path = {
    val url = (architecture, os) match {
      case ("arm64", "darwin")  => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-darwin-arm64.tar.gz"
      case ("amd64", "darwin")  => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-darwin-x64.tar.gz"
      case ("amd64", "windows") => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-win-x64.zip"
      case ("amd64", "linux")   => s"https://nodejs.org/dist/v$nodeVersion/node-v$nodeVersion-linux-x64.tar.gz"
      case (arch, os)           => throw new BleepException.Text(s"Unsupported combination of architecture $arch and os $os")
    }
    val fileCache = FileCache[Task]().withLogger(logger)
    val cache = ArchiveCache[Task]().withCache(fileCache)

    Await.result(cache.get(Artifact(url)).value(ec), Duration.Inf) match {
      case Left(value) => throw new BleepException.Cause(value, s"couldn't download node $nodeVersion from url $url")
      case Right(folder) =>
        val nodeBin = folder.toPath / folder.getName.replace(".tar.gz", "").replace(".zip", "") / (if (os == "windows") "node.exe" else "bin/node")
        if (!nodeBin.toFile.exists()) {
          sys.error(s"Expected $nodeBin to exist")
        }
        nodeBin
    }
  }
}
