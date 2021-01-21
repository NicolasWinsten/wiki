package com.nicolaswinsten.wiki

import java.util.Date

import Scraper._
import Page._
import Title._
import com.nicolaswinsten.util._

sealed abstract class Page(t: Title) {
  protected val doc: Document = fetchDoc(t) match {
    case Some(doc) => doc
    case None => throw new IllegalArgumentException(s"$t is not a valid title")
  }

  val title: Title = extractTitle(doc)
  def content: String = doc.toHtml

  override def equals(obj: Any): Boolean = obj match {
    case other: Page => this.title == other.title
    case _ => false
  }

  override def hashCode(): Int = title.hashCode()

  override def toString: String = title.name

  def scrape(pattern: String, group: Int): Iterator[String] = collect(doc)(pattern, group)

  lazy val outLinks: List[Title] = scrape("<a href=\"/wiki/([^\"]+)\"", 1) map stringToTitle to List
  lazy val inLinks = ???

  lazy val normalCategories: List[Title] = extractCategories(doc) map stringToTitle
  lazy val hiddenCategories: List[Title] = extractHiddenCategories(doc) map stringToTitle

  lazy val lastEditDate: Date = extractEditDate(doc)

  def refreshed: Page = instantiatePage(title) // TODO is this needed?
}

object Page {
  private val cache = scala.collection.mutable.WeakHashMap[Title, Page]()

  def apply(title: Title): Page = {
    unless (cache contains title) { cache += (title -> instantiatePage(title)) }
    cache(title)
  }

 private def instantiatePage(title: Title): Page = {
   import Namespaces._
   resolveRedirect(title).ns match {
     case Main => new Article(title)
     case Category => new Category(title)
     case _ => throw new NotImplementedError(s"Titles of namespace ${title.ns} are not handled yet")
   }
 }

  def random: Article = new Article("Special:Random")
}

class Article(title: Title) extends Page(title) {

}

class Category(title: Title) extends Page(title) {
  lazy val subcategories: List[Title] = extractSubcategories(doc) map stringToTitle
  lazy val members: List[Title] = extractCategoryMembers(doc) map stringToTitle

  /**
   * Fetch the members of this Category. Use this to attach a limit to the number of fetch requests allowed in order
   * to ensure a reasonable return time.
   * @param fetchLimit maximum number of fetch requests allowed for the query
   */
  def getMembers(fetchLimit: Int): List[String] = extractCategoryMembers(doc, fetchLimit)
}

class Portal(title: Title) extends Page(title) {
  lazy val relatedPortals = ???
}

object Test extends App {

  def invCategory(category: Category): Unit = {
    category.subcategories foreach ( m => {
      print(m)
      println(s" is a sub of $category")
    }
    )

    for (c <- category.normalCategories) invCategory(new Category(c))
  }

  invCategory(new Category("Category:United States"))
}