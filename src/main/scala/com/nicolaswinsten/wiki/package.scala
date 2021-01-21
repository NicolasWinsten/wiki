package com.nicolaswinsten

/**
 * Provides classes for reading Wikipedia pages.
 *
 * {{{
 *   Page("TITLE") // produces a Page object representing http://en.wikipedia.org/wiki/TITLE
 * }}}
 *
 * [[Title]] objects represent the titles for wikipedia pages. Use these to filter/match by namespace.
 */
package object wiki {

  def unless(condition: => Boolean)(body: => Unit): Unit = if (!condition) body
}
