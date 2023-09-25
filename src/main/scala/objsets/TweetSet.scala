package objsets

import TweetReader.*

class Tweet(val user: String, val text: String, val retweets: Int):

  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

abstract class TweetSet extends TweetSetInterface:

  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException("Set is empty")

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def descendingByRetweet: TweetList = Nil

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    val accValue = if p(elem) then acc.incl(elem) else acc
    left.filterAcc(p, right.filterAcc(p, accValue))

  def union(that: TweetSet): TweetSet =
    left.union(right.union(that)).incl(elem)

  def mostRetweeted: Tweet =
    var hasMoreRetweets = elem
    foreach(that => hasMoreRetweets = if that.retweets > hasMoreRetweets.retweets then that else hasMoreRetweets)
    hasMoreRetweets

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

  def descendingByRetweet: TweetList =
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)

trait TweetList:
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")

  def tail = throw java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false

object GoogleVsApple:
  private val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  private val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  private lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(keyword => tweet.text.contains(keyword)))
  private lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(keyword => tweet.text.contains(keyword)))

  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  GoogleVsApple.trending foreach println
