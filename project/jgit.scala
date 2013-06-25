import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.api.{Git => PGit}
import org.eclipse.jgit.lib.Ref
import sbt._

// TODO - once this is in the sbt-git plugin, remove it.
class GitRepository(val repo: Repository) {
  val porcelain = new PGit(repo)

  def headCommit = Option(repo.resolve("HEAD")) map (_.name)

  def tagHash(tag: Ref) = {
    // Annotated (signed) and plain tags work differently,
    // plain ones have the null PeeledObjectId
    val peeled = repo.peel(tag)
    val id =
      if (peeled.getPeeledObjectId ne null)
        peeled.getPeeledObjectId
      else
        peeled.getObjectId
    id.getName
  }

  def currentTags: Seq[String] = {
    import collection.JavaConverters._
    val list = porcelain.tagList.call().asScala
    for {
      hash <- headCommit.toSeq
      unpeeledTag <- list
      taghash = tagHash(unpeeledTag)
      if taghash == hash
      ref = unpeeledTag.getName
      if ref startsWith "refs/tags/"
    } yield ref drop 10
  }
}
object jgit {
  /** Creates a new git instance from a base directory. */
  def apply(base: File) = new GitRepository({
    val gitDir = new File(base, ".git")
    new FileRepositoryBuilder().setGitDir(gitDir)
      .readEnvironment() // scan environment GIT_* variables
     .findGitDir() // scan up the file system tree
     .build()
  })  
}


