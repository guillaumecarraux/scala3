package scala.annotation

import java.lang.annotation.{Target}

/** An annotation that indicates that an element will never be used again after this evaluation,
    and can be removed from the stack by force
 */
final class lastUse extends scala.annotation.StaticAnnotation
