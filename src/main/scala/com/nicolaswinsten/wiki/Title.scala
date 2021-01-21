package com.nicolaswinsten.wiki

import Namespaces.Namespace

final case class Title(title: String, ns: Namespace) {
  def name = s"$ns${ if (ns != Namespaces.Main) ":" else "" }$title"
  override def toString: String = name
  def getPage: Page = Page(this)
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
  def apply(fullTitle: String): Title = {
    val ns = extractNamespace(fullTitle)
    new Title(fullTitle stripPrefix s"$ns:", ns)
  }

  def apply(simpleTitle: String, ns: Namespace) = new Title(simpleTitle, ns)

  def apply(simpleTitle: String, ns: Int): Title =
    try new Title(simpleTitle, Namespaces(ns))
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

  implicit def stringToTitle(name: String): Title = Title(name replaceAll (" ", "_"))
}