package org.biancama.algorithms.trampoline

import org.biancama.algorithms.trampoline.WordsTrampoline.callExplore

import scala.annotation.tailrec
import scala.util.control.TailCalls._
import scala.io.Source._
object WordsTrampoline {
  def explore(count: Int, words: List[String]) : TailRec[Int] =
    if(words.size == 0)
      done(count)
    else
      tailcall(countPalindrome(count, words))

  def countPalindrome(count: Int, words: List[String]) : TailRec[Int]  = {
    val firstWord = words.head

    if(firstWord.reverse == firstWord)
      tailcall(explore(count + 1, words.tail))
    else
      tailcall(explore(count, words.tail))
  }

  def callExplore(text: String) =
    println(explore(0, text.split(" ").toList).result)


  def main(args: Array[String]): Unit = {
    callExplore("dad mom and racecar")

    try {
      val text =
        fromURL("https://en.wikipedia.org/wiki/Gettysburg_Address").mkString
      callExplore(text)
    } catch {
      case ex : Throwable => println(ex)
    }
  }

}

object Words {
  def explore(count: Int, words: List[String]) : Int =
    if(words.size == 0)
      count
    else
      countPalindrome(count, words)

  def countPalindrome(count: Int, words: List[String]) : Int  = {
    val firstWord = words.head

    if(firstWord.reverse == firstWord)
      explore(count + 1, words.tail)
    else
      explore(count, words.tail)
  }

  def callExplore(text: String) = println(explore(0, text.split(" ").toList))
  def main(args: Array[String]): Unit = {
    callExplore("dad mom and racecar")

    try {
      val text =
        fromURL("https://en.wikipedia.org/wiki/Gettysburg_Address").mkString
      callExplore(text)
    } catch {
      case ex : Throwable => println(ex)
    }
  }
}

