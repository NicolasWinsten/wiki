package com.nicolaswinsten.wiki

import java.util.Date

import Scraper._
import Title._
import Namespaces.Namespace

sealed abstract class Page (protected val doc: Document) {
  /**
   * Namespace of this page
   */
    // note that this field is lazy and throws an exception. This is because it is used to check if a Page is constructed
    // with the appropriate Document (Category pages must be given a Category Document). If it were not lazy, then
    // the requirement would fail because it would be initialized to null while constructing. See: https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
  lazy val ns: Namespaces.Namespace = throw new RuntimeException("Nick, you must override this in subclasses")
  val title: Title = extractTitle(doc)
  require(title.ns == ns, s"$title did not match namespace $ns") // catch if the given document does not match namespace

  /**
   * @return html String for this Page
   */
  def content: String = doc.toHtml

  override def equals(obj: Any): Boolean = obj match {
    case other: Page => this.title == other.title
    case _ => false
  }

  override def hashCode(): Int = title.hashCode()

  override def toString: String = title.name

  /**
   * Scrape this Page for the given pattern and return all matches
   * @param pattern regex
   * @param group group number in pattern to return, default is whole pattern
   */
  def scrape(pattern: String, group: Int = 0): Iterator[String] = collect(doc)(pattern, group)

  lazy val outLinks: List[Title] = scrape("<a href=\"/wiki/([^\"]+)\"", 1) map string2Title to List
  lazy val inLinks = ???

  lazy val normalCategories: List[Title] = extractCategories(doc) map string2Title
  lazy val hiddenCategories: List[Title] = extractHiddenCategories(doc) map string2Title

  lazy val lastEditDate: Date = extractEditDate(doc)
}

object Page {
  // store pages that have already been fetched
  private val cache = scala.collection.mutable.WeakHashMap[Title, Page]()

  /**
   * Returns an instance of the correct Page subclass given a Wikipedia title
   * @param title Wikipedia title
   * @return Page
   */
  def apply(title: Title): Page = cache.getOrElseUpdate(title, instantiatePage(title))

 private def instantiatePage(title: Title): Page = {
   val doc = fetchDoc(title)
   extractTitle(doc).ns match {
     case Namespaces.Main => Article(doc)
     case Namespaces.Category => Category(doc)
     case _ => throw new NotImplementedError(s"Titles of namespace ${title.ns} are not handled yet")
   }
 }

  /**
   * @return a random Article page
   */
  def randomArticle: Article = Article("Special:Random")

  implicit def title2doc(title: Title): Document = fetchDoc(title)
  implicit def string2doc(title: String): Document = fetchDoc(string2Title(title))
}


case class Article (override val doc: Document) extends Page(doc) {
  lazy override val ns: Namespace = Namespaces.Main
}

case class Category (override val doc: Document) extends Page(doc) {
  lazy override val ns: Namespace = Namespaces.Category
  lazy val subcategories: List[Title] = extractSubcategories(doc) map string2Title
  /**
   * @note May take a long time to return depending on how large the category is. Use getMembers instead
   */
  lazy val members: List[Title] = extractCategoryMembers(doc) map string2Title

  /**
   * Fetch the members of this Category. Use this to attach a limit to the number of fetch requests allowed in order
   * to ensure a reasonable return time.
   * @param fetchLimit maximum number of fetch requests allowed for the query
   */
  def getMembers(fetchLimit: Int): List[String] = extractCategoryMembers(doc, fetchLimit)
}

