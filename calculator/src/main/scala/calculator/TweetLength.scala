package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Signal(MaxTweetLength -  count(tweetText()))
  }

 def count(buf: String): Int = {
   var i: Int = 0
   var n: Int = buf.length
   val endIndex: Int = buf.length
   while (i < endIndex) {
     i+=1;
     if (isHighSurrogate(buf.charAt(i - 1)) && i < endIndex && isLowSurrogate(buf.charAt(i))) {
       n -= 1
       i += 1
     }
   }
   n
 }

  def isHighSurrogate(ch: Char) : Boolean = {
    return (ch >= '\uD800') && (ch < ('\uDBFF' + 1))
  }

  def isLowSurrogate (ch: Char): Boolean =  {
    return ch >= '\uDC00' && ch < ('\uDFFF' + 1)
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    Signal(if (remainingCharsCount() < 0)
      "red"
    else if (remainingCharsCount() < 15)
      "orange"
    else "green")
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
