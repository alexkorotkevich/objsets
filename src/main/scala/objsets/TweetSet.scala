package objsets

import TweetReader.*

class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

abstract class TweetSet extends TweetSetInterface:
  def filter(p: Tweet => Boolean): TweetSet

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def isEmpty: Boolean

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:
  def filter(p: Tweet => Boolean): TweetSet = this

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  def isEmpty = true

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def descendingByRetweet: TweetList = Nil

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:
  def filter(p: Tweet => Boolean): TweetSet =
    this.filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    if p(elem) then left.filterAcc(p, right.filterAcc(p, acc.incl(elem)))
    else left.filterAcc(p, right.filterAcc(p, acc))

  def union(that: TweetSet): TweetSet =
    left.union(right.union(that)).incl(elem)

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

  def mostRetweeted: Tweet = ???

  def isEmpty = false

  def descendingByRetweet: TweetList = ???
//  if isEmpty then Nil
//  else
//    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)

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
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(item => tweet.text.contains(item)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(item => tweet.text.contains(item)))
  //  val filteredTweets: TweetSet = TweetReader.allTweets.filter(tweet => tweet.retweets > 10)
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = ???

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
//  println(GoogleVsApple.filteredTweets)
//  GoogleVsApple.filteredTweets foreach println

