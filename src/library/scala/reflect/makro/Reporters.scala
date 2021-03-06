package scala.reflect.makro

trait Reporters {
  self: Context =>

  import mirror._

  /** Exposes means to control the compiler UI */
  def reporter: Reporter
  def setReporter(reporter: Reporter): this.type
  def withReporter[T](reporter: Reporter)(op: => T): T

  /** For sending a message which should not be labeled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   *  Use ``enclosingPosition'' if you're in doubt what position to pass to ``pos''.
   */
  def echo(pos: Position, msg: String): Unit

  /** Informational messages, suppressed unless -verbose or force=true.
   *  Use ``enclosingPosition'' if you're in doubt what position to pass to ``pos''.
   */
  def info(pos: Position, msg: String, force: Boolean): Unit

  /** Warnings and errors.
   *  Use ``enclosingPosition'' if you're in doubt what position to pass to ``pos''.
   */
  def hasWarnings: Boolean
  def hasErrors: Boolean
  def warning(pos: Position, msg: String): Unit
  def error(pos: Position, msg: String): Unit

  /** Abruptly terminates current macro expansion leaving a note about what happened.
   *  Use ``enclosingPosition'' if you're in doubt what position to pass to ``pos''.
   */
  def abort(pos: Position, msg: String): Nothing

  /** Drops into interactive mode if supported by the compiler UI */
  def interactive(): Unit
}