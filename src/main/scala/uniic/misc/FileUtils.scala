package uniic.misc
import java.io.File

trait FileUtils {
  implicit def fileOps(f: File) = new {
    def /(name: String) = new File(f.getPath + File.separator + name)
  }
}

object FileUtils extends FileUtils
