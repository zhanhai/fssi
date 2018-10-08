package fssi
package scp
package interpreter

import org.slf4j._

trait LogSupport {
  implicit val log = LoggerFactory.getLogger(getClass)
}
