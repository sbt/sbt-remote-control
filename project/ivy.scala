import sbt._
import Keys.{target, resolvers, publishLocal, ivySbt, streams, state, resolvedScoped}
import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.install.InstallOptions
import org.apache.ivy.plugins.matcher.PatternMatcher
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.core.resolve.IvyNode
import collection.JavaConverters._
import java.io.BufferedWriter
import org.apache.ivy.core.module.id.ModuleId
import Project.Initialize



object IvyRepositories {

  // Here are the keys used to resolve local artifacts into an integration testing repo.
  val localArtRepoName = "install-to-local-project-repository"
  val localArtRepo = SettingKey[File]("local-art-repository", "The location to install our projects.")
  val localArtPublished = TaskKey[Unit]("local-art-published", "A task which will publish our local artifacts.")
  val localArtRepoCreation = TaskKey[File]("local-art-repository-creation", "Creates a local repository in the specified location.")

  // Here are the keys used to resolve remote artifacts into an integration testing repo.
  val localRepoArtifacts = SettingKey[Seq[ModuleID]]("local-repository-artifacts", "Artifacts included in the local repository.")
  val localDepRepoName = "install-to-local-dep-repository"
  val localDepRepo = SettingKey[File]("local-dep-repository", "The location to install a local repository of our dependencies.")
  val localDepRepoCreation = TaskKey[LocalRepoReport]("local-dep-repository-creation", "Creates a local repository in the specified location.")
  val localDepRepoLicenses = TaskKey[Unit]("local-repository-licenses", "Prints all the licenses used by software in the local repo.")
  val localDepRepoCreated = TaskKey[File]("local-dep-repository-created", "Creates a local repository in the specified location.")

  // This task joins together the dependency repo and the remote artifact repo.
  val localRepo = SettingKey[File]("local-repository", "The location of the local repository..")
  val localRepoCreated = TaskKey[File]("local-repository-created", "Creates a local repository in the specified location.")
  
  import sbinary.DefaultProtocol.ClassFormat

  // This method caches *ONCE PER RELOAD* a task.  TODO - We should add hooks to detect
  // changes in dependencies that require us to reload....
  def useCacheOr[T](key: TaskKey[T], staleCheck: Initialize[Task[Boolean]], action: Initialize[Task[T]]): Initialize[Task[T]] = {
    val withStaleCheck: Initialize[Task[Option[T]]] =
      getPrevious(key).zipWith(staleCheck) { (previous, staleCheck) =>
        staleCheck flatMap { flag =>
          if(flag) task(None) else previous
        }
      }
    withStaleCheck.zipWith(action) { (previous, current) =>
      previous flatMap {
        case Some(value) => task(value)
        case None => current
      }
    } keepAs key  // TODO - Why isn't this saving...
  }

  def makeLocalDepRepo: Initialize[Task[LocalRepoReport]] =
    (localDepRepo, localRepoArtifacts, ivySbt, streams, state) map { (r, m, i, s, state) =>
      val licenses = IvyHelper.createLocalRepository(m, localDepRepoName, i, s.log)
      val result = LocalRepoReport(r, licenses)
      result
    }

  def publishProjectTo(project: ProjectReference): Initialize[Task[Unit]] =
    (Keys.publishLocalConfiguration in project, Keys.moduleSettings in project, Keys.ivySbt, streams) map rejiggerPublishing

  def rejiggerPublishing(config: PublishConfiguration, moduleSettings: ModuleSettings, ivy: IvySbt, s: Keys.TaskStreams): Unit = {
    val module =
        new ivy.Module(moduleSettings)
      val newConfig =
         new PublishConfiguration(
             config.ivyFile,
             localArtRepoName,
             config.artifacts,
             config.checksums,
             config.logging)
      s.log.info("Publishing " + module + " to local repo: " + localArtRepoName)
      IvyActions.publish(module, newConfig, s.log)
  }
  
  def makeLocalRepoSettings(publishedProjects: Seq[Project]): Seq[Setting[_]] = Seq(
    localDepRepo <<= target(_ / "local-dep-repository"),
    localArtRepo <<= target(_ / "local-art-repository"),
    localRepo <<= target(_ / "local-repository"),
    localRepoArtifacts <<= {
      val projectDeps: Seq[Initialize[Seq[ModuleID]]] =
        publishedProjects map (Keys.libraryDependencies in _)
      projectDeps reduce { (left, right) =>
        left.zipWith(right) { _ ++ _ }
      }
    },
    resolvers <+= localDepRepo apply { f => Resolver.file(localDepRepoName, f)(Resolver.ivyStylePatterns) },
    resolvers <+= localArtRepo apply { f => Resolver.file(localArtRepoName, f)(Resolver.ivyStylePatterns) },
    localDepRepoCreation <<= useCacheOr(
        key = localDepRepoCreation,
        staleCheck = (localDepRepo) map (_.isDirectory),
        action = makeLocalDepRepo),
    localDepRepoCreated <<= localDepRepoCreation map (_.location),
    localDepRepoLicenses <<= (localDepRepoCreation, streams) map { (config, s) =>
      // Stylize the licenses we used and give an inline report...
      s.log.info("--- Licenses ---")
      val badList = Set("and", "the", "license", "revised")
      def makeSortString(in: String): String =
        in split ("\\s+") map (_.toLowerCase) filterNot badList mkString ""
      for(license <- config.licenses sortBy (l => makeSortString(l.name))) {
        s.log.info(" * " + license.name + " @ " + license.url)
         s.log.info("    - " + license.deps.mkString(", "))
      }
    },
    // Here we hack our projects to publish to a repository of our choosing.
    localArtPublished <<= {
      val publishProjects: Seq[Initialize[Task[Unit]]] =
        publishedProjects map (p => publishProjectTo(p))
      val tmp: Initialize[Task[Unit]] =
        (publishProjects).reduce { (l, r) =>
          l.zipWith(r) { _ && _ }
        }
      tmp
    },
    localArtRepoCreation <<= (localArtPublished, localArtRepo) apply { (task, file) =>
      task map (_ => file)
    },
    localRepoCreated <<= (localDepRepoCreated, localArtRepoCreation, localRepo) map { (art, dep, full) =>
      def collectRepoFiles(repo: File): Seq[(File, File)] =
        for {
          (file, name) <- (repo.*** --- repo) x relativeTo(repo)
        } yield file -> (full / name)
      IO.copy(collectRepoFiles(art) ++ collectRepoFiles(dep))
      full
    }
  )
}



package sbt {
  object IvySbtCheater {
    def toID(m: ModuleID) = IvySbt toID m
  }
}

case class License(name: String, url: String)(val deps: Seq[String]) {
  override def toString = name + " @ " + url
}
case class LocalRepoReport(location: File, licenses: Seq[License])

object IvyHelper {
  
  /** Resolves a set of modules from an SBT configured ivy and pushes them into
   * the given repository (by name).
   * 
   * Intended usage, requires the named resolve to exist, and be on that accepts installed artifacts (i.e. file://)
   */
  def createLocalRepository(
      modules: Seq[ModuleID],
      localRepoName: String,
      ivy: IvySbt, 
      log: Logger): Seq[License] = ivy.withIvy(log) { ivy =>



    // This helper method installs a particular module and transitive dependencies.
    def installModule(module: ModuleID): Option[ResolveReport] = {
      // TODO - Use SBT's default ModuleID -> ModuleRevisionId
      val mrid = IvySbtCheater toID module
      val name = ivy.getResolveEngine.getSettings.getResolverName(mrid)
      log.debug("Module: " + mrid + " should use resolver: " + name)
      try Some(ivy.install(mrid, name, localRepoName,
                new InstallOptions()
                    .setTransitive(true)
                    .setValidate(true)
                    .setOverwrite(true)
                    .setMatcherName(PatternMatcher.EXACT)
                    .setArtifactFilter(FilterHelper.NO_FILTER)
                ))
       catch {
         case e: Exception =>
           log.debug("Failed to resolve module: " + module)
           log.trace(e)
           None
       }
    }
    // Grab all Artifacts
    val reports = (modules flatMap installModule).toSeq
    
    dumpDepGraph(reports)
    
    val licenses = for {
      report <- reports
      dep <- report.getDependencies.asInstanceOf[java.util.List[IvyNode]].asScala
      if dep != null
      desc <- Option(dep.getDescriptor).toSeq
      license <- Option(desc.getLicenses) getOrElse Array.empty
    } yield License(license.getName, license.getUrl)(dep.getAllArtifacts.map(_.getName))
    
    // Create reverse lookup table for licenses by artifact...
    val grouped = for {
      (name, licenses) <- licenses.groupBy(_.name)
      l <- licenses.headOption.toSeq
    } yield License(l.name, l.url)(licenses flatMap (_.deps) distinct)
    
    grouped.toIndexedSeq
  }
  
  def withPrintableFile(file: File)(f: (Any => Unit) => Unit): Unit = {
    IO.createDirectory(file.getParentFile)
    Using.fileWriter(java.nio.charset.Charset.defaultCharset, false)(file) { writer =>
      def println(msg: Any): Unit = {
        System.out.println(msg)
        writer.write(msg.toString)
        writer.newLine()
      }
      f(println _)
    }
  }
  
    // TODO - Clean this up and put it somewhere useful.
  def dumpDepGraph(reports: Seq[ResolveReport]): Unit = withPrintableFile(new File("target/local-repo-deps.txt")) { println =>
    // Here we make an assumption...
    // THE FIRST MODULE is the one that we wanted, the rest are
    // the ones we pulled in...
    for((report, id) <- reports.zipWithIndex) {
      val modules = report.getModuleIds.asInstanceOf[java.util.List[ModuleId]].asScala
      val requested = modules.head
      val name = requested.getOrganisation + ":" + requested.getName
      println(name + " - requested")
      // Now find what we got:
      val deps = for {
	    dep <- report.getDependencies.asInstanceOf[java.util.List[IvyNode]].asScala
	    if dep != null
	    depId = dep.getId
	    if !((depId.getOrganisation == requested.getOrganisation) && (depId.getName == requested.getName))
	  } yield depId.getOrganisation + ":" + depId.getName + ":" + depId.getRevision
	  
	  deps foreach { dep => println("\t " + dep) }
    }
  }
  
}