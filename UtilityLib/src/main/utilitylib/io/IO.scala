package utilitylib.io

import java.io.{FileWriter, File, PrintWriter}
import java.net.{URL, URI}
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/30/12
 * Time: 1:54 PM
 */

object IO {

  implicit private def slurpedStream2String(stream: Stream[String]) : String = stream.mkString("\n")

  def appendToFile(fileName: String)(data: => String) { using (new FileWriter(fileName, true))(fileWriter => using (new PrintWriter(fileWriter))(printWriter => printWriter.println(data))) }
  
  def printToFile(f: File)(op: PrintWriter => Unit)          { val p = new PrintWriter(f); try { op(p) } finally { p.close() } }
  def printToFile(filename: String)(op: PrintWriter => Unit) { printToFile(new File(filename))(op) }
  def printToFile(filename: String)(data: => String)         { using (new FileWriter(filename))(fileWriter => fileWriter.write(data)) }

  // If you want to read from a source, you'll generally just want to call `slurp`/`slurpFrom_Path`
  def slurp(file: File)                        : String         = slurpLines(file)
  def slurp(uri: URI)                          : String         = slurpLines(uri)
  def slurp(url: URL)                          : String         = slurpLines(url)
  def slurpFromFilePath(filePath: String)      : String         = slurpLinesFromFilePath(filePath)
  def slurpFromURIPath(uriPath: String)        : String         = slurpLinesFromURIPath(uriPath)
  def slurpFromURLPath(urlPath: String)        : String         = slurpLinesFromURLPath(urlPath)
  def slurpLines(file: File)                   : Stream[String] = slurpLinesFromSource(Source.fromFile(_: File), file)
  def slurpLines(uri: URI)                     : Stream[String] = slurpLinesFromSource(Source.fromURI(_: URI), uri)
  def slurpLines(url: URL)                     : Stream[String] = slurpLinesFromSource(Source.fromURL(_: URL), url)
  def slurpLinesFromFilePath(filePath: String) : Stream[String] = slurpLines(new File(filePath))
  def slurpLinesFromURIPath(uriPath: String)   : Stream[String] = slurpLines(new URI(uriPath))
  def slurpLinesFromURLPath(urlPath: String)   : Stream[String] = slurpLines(new URL(urlPath))

  def using[A <: { def close() : Unit }, B](param: A)(f: A => B) : B = try { f(param) } finally { param.close() }

  private def slurpLinesFromSource[T](f: (T) => io.BufferedSource, t: T) : Stream[String] = {
    val src = f(t)
    val stream = src.getLines().toStream
    src.close()
    stream
  }

}
