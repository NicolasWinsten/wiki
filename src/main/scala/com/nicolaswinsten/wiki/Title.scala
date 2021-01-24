package com.nicolaswinsten.wiki

import Namespaces.Namespace

/**
 * Title objects represent Wikipedia titles.
 *
 * @example `Title("Literature", Namespaces.Portal)` produces the title Portal:Literature
 *
 * @param simpleTitle the simple name for a title not including namespace
 * @param ns namespace of the title
 */
final case class Title(simpleTitle: String, ns: Namespace) {
  /**
   * @return full specified name for this Title
   */
  def name = s"$ns${ if (ns != Namespaces.Main) ":" else "" }$simpleTitle"
  override def toString: String = name
}


object Namespaces extends Enumeration {
  type Namespace = Value

  // ids for each Value are given by Wikipedia
  val Main:       Namespace = Value(0, "")
  val User:       Namespace = Value(2, "User")
  val Wikipedia:  Namespace = Value(4, "Wikipedia")
  val File:       Namespace = Value(6, "File")
  val MediaWiki:  Namespace = Value(8, "MediaWiki")
  val Template:   Namespace = Value(10, "Template")
  val Help:       Namespace = Value(12, "Help")
  val Category:   Namespace = Value(14, "Category")
  val Portal:     Namespace = Value(100, "Portal")
  val Draft:      Namespace = Value(118, "Draft")

  val Talk:           Namespace = Value(1, "Talk")
  val User_talk:      Namespace = Value(3, "User talk")
  val Wikipedia_talk: Namespace = Value(5, "Wikipedia talk")
  val File_talk:      Namespace = Value(7, "File talk")
  val MediaWiki_talk: Namespace = Value(9, "MediaWiki talk")
  val Template_talk:  Namespace = Value(11, "Template talk")
  val Help_talk:      Namespace = Value(13, "Help talk")
  val Category_talk:  Namespace = Value(15, "Category talk")
  val Portal_talk:    Namespace = Value(101, "Portal talk")
  val Draft_talk:     Namespace = Value(119, "Draft talk")

  val Talks = Set(
    Talk, User_talk, Wikipedia_talk, File_talk, MediaWiki_talk, Template_talk, Help_talk, Category_talk,
    Portal_talk, Draft_talk
  )
}

/**
 * Titles are the names of Wikipedia entries. Titles include a name and its namespace.
 */
object Title {
  /**
   * Return the Title with the given full name
   * @param fullTitle full title including namespace
   */
  def apply(fullTitle: String): Title = {
    val ns = extractNamespace(fullTitle)
    Title(fullTitle stripPrefix s"$ns:", ns)
  }

  /**
   * Return a Title given a simple name and namespace ID
   * @param simpleTitle title without namespace
   * @param ns integer id for namespace
   */
  def apply(simpleTitle: String, ns: Int): Title =
    try Title(simpleTitle, Namespaces(ns))
    catch {
      case _: NoSuchElementException => throw new NoSuchElementException(
        s"$ns is not a valid id for a Wikipedia namespace. Valid ids can be found here: " +
          s"https://en.wikipedia.org/wiki/Wikipedia:Namespace"
      )
    }

  // extract the Namespace from the String of a full Wikipedia title
  private def extractNamespace(title: String): Namespace = {
    try Namespaces.withName(title.split(":")(0))
    catch {
      case _: NoSuchElementException => Namespaces.Main
    }
  }

  implicit def string2Title(name: String): Title = Title(name replaceAll (" ", "_"))
}