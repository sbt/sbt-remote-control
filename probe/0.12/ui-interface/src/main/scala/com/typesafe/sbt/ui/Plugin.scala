package com.typesafe.sbt.ui

/**
 * This UI plugin provides the basic settings used by plugins that want to be able to communicate with a UI.
 *
 * Basically, we just stub out the setting you can use to look up the current UI context.
 */
object SbtUiPlugin extends sbt.Plugin {

  val uiContext = sbt.SettingKey[Context]("sbt-ui-context", "The context used to communicate to a user interface running sbt.")

  override val buildSettings: Seq[sbt.Setting[_]] = Seq(uiContext in sbt.Global <<= (uiContext in sbt.Global) ?? Context.noop)

  import sbt._
  import Keys._
  def foo: Setting[_] =
    fullResolvers <<= (fullResolvers, bootResolvers, appConfiguration) map {
      case (rs, Some(b), app) =>
        def getResolvers(app: xsbti.AppConfiguration): Option[Seq[xsbti.Repository]] =
          try Some(app.provider.scalaProvider.launcher.ivyRepositories.toSeq)
          catch { case _: NoSuchMethodError => None }
        def findLocalResolverNames(resolvers: Seq[xsbti.Repository]): Seq[String] =
          for {
            r <- resolvers
            if r.isInstanceOf[xsbti.IvyRepository]
            ivy = r.asInstanceOf[xsbti.IvyRepository]
            if ivy.url.getProtocol == "file"
          } yield ivy.id
        val newResolvers: Seq[Resolver] =
          getResolvers(app).map(findLocalResolverNames).getOrElse(Nil).flatMap(name => b.find(_.name == name))
        newResolvers ++ rs
      case (rs, _, _) => rs
    }
}
