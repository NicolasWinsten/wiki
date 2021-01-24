package com.nicolaswinsten.wiki

import java.net.URL
import java.text.SimpleDateFormat
import java.util.Date

import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import org.jsoup.HttpStatusException

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Utility object for scraping the HTML of a Wikipedia page
 */
private[wiki] object Scraper {
  val browser: Browser = JsoupBrowser()
  type Document = browser.DocumentType
  private val url = "https://en.wikipedia.org"

  // fetch the Document from an arbitrary URL
  private def fetchDocByUrl(url: String): Document = browser parseInputStream new URL(url).openStream()

  /**
   * Fetch the Wikipedia page Document for a given title
   * @param title Wikipedia title
   */
  def fetchDoc(title: Title): Document =
    try fetchDocByUrl(s"$url/wiki/$title")
    catch {
      case _: HttpStatusException => throw new IllegalArgumentException(s"$title did not produce a Wikipedia page")
    }

  /**
   * Extract the title of the given Wikipedia document
   * @param doc Wikipedia page
   */
  def extractTitle(doc: Document): Title = Title(doc.title stripSuffix " - Wikipedia")

  /**
   * Returns group <var>i</var> in matches of <var>pattern</var> in <var>doc</var>
   * @param doc Wikipedia page
   * @param pattern regex pattern to search for
   * @param i group in pattern to capture, defaults to 0 (whole pattern)
   */
  def collect(doc: Document)(pattern: String, i: Int = 0): Iterator[String] = {
    val html = doc.toHtml; val regex = pattern.r
    val matches = regex findAllMatchIn html
    matches map { _ group i }
  }


  /**
   * Returns the String titles for the normal categories the given wiki page belongs to
   * @param doc Document of a Wikipedia page
   */
  def extractCategories(doc: Document): List[String] =
    doc >?> element("#mw-normal-catlinks") match {
      case Some(x) => x >> elementList("li > a") >> attr("href") map (_ stripPrefix "/wiki/" )
      case None => Nil
    }

  /**
   * Returns the String titles for the hidden categories the given wiki page belongs to
   * @param doc Document of a Wikipedia page
   */
  def extractHiddenCategories(doc: Document): List[String] =
    doc >?> element("#mw-hidden-catlinks") match {
      case Some(x) => x >> elementList("li > a") >> attr("href") map (_ stripPrefix "/wiki/" )
      case None => Nil
    }

  /**
   * Returns the String titles for the subcategories of the given Category page
   * @param doc Document of a Wikipedia Category page
   */
  def extractSubcategories(doc: Document): List[String] =
    doc >?> element("#mw-subcategories") match {
      case Some(x) => x >> elementList("div.CategoryTreeItem > a") >> attr("href") map (_ stripPrefix "/wiki/" )
      case None => Nil
    }

  // helper function for extracting category members. recursively builds a list of titles that are category members
  // of the given Category page Document
  @tailrec
  private def _extractCategoryMembers(doc: Document, limit: Int, soFar: ListBuffer[String]): List[String] = {
    val members = doc >?> element("#mw-pages .mw-content-ltr") match {
      case Some(x) => x >> elementList("a") >> attr("href") map (_ stripPrefix "/wiki/" )
      case None => Nil
    }

    soFar addAll members

    // Category pages with a lot of members will contain a link to a "next page" with more.
    // follow the link to the next page to get more category members
    val link = ((doc >> elementList("#mw-pages > a")) find (a => a.text == "next page")) >?> attr("href")

    if (limit <= 1 || link.isEmpty) soFar.toList
    else _extractCategoryMembers(fetchDocByUrl(url + link.get.get), limit - 1, soFar)
  }

  /**
   * Returns a list of String titles of the member pages for the given category page
   * @param doc Document to a Wikipedia Category page
   * @param limit maximum number of document fetches allowed, defaults to no limit
   * @note If no limit is given, it may take a very long time for this method to return given certain pages
   */
  def extractCategoryMembers(doc: Document, limit: Int = Int.MaxValue): List[String] =
    _extractCategoryMembers(doc, limit, ListBuffer())


  /**
   * Returns the last edit date of the given wikipedia page in local time
   * @param doc Document of a Wikipedia page
   */
  def extractEditDate(doc: Document): Date = {
    val dateMatch = "(\\d{1,2}) (\\w*) (\\d{4}), at (\\d{2}):(\\d{2}) \\((\\w{3})\\)".r findFirstMatchIn doc >> text("#footer-info-lastmod")
    val dateStr = dateMatch.get.subgroups mkString ":"
    new SimpleDateFormat("dd:MMMM:yyyy:hh:mm:z") parse dateStr
  }

}
