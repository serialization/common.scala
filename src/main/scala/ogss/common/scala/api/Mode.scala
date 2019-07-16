package ogss.common.scala.api

/**
 * Modes for file handling.
 */
sealed abstract class Mode;
sealed abstract class ReadMode extends Mode;
sealed abstract class WriteMode extends Mode;
object Create extends ReadMode;
object Read extends ReadMode;
object Write extends WriteMode;
/**
 * can not be written at all; read only is permanent and must not be changed with a change mode
 */
object ReadOnly extends WriteMode;