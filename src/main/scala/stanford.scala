package gpp.classify

import scala.xml._

object Stanford {

  def main(args: Array[String]) {

   val in = io.Source.fromFile(args(0)).getLines.toList

   val correct_input = in.map{rawLine =>
      if(rawLine(0)=='0') "negative"+rawLine.drop(1)
      else if (rawLine(0)=='2') "neutral"+rawLine.drop(1)
      else "positive"+rawLine.drop(1)
    }

   val split_in = correct_input.map(line => line.split(";;"))

  
   val xml = <dataset>{split_in.map(x => 
                <item label={ x(0) } tweetid={ x(1) } date={ x(2) } target={ x(3) } username={ x(4) }>
                  <content>{ x(5) }</content>
                </item>
              )}</dataset>

  println(xml)
 }


}