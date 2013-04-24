package gpp.classify

import scala.xml._

object Emote {

  def main(args: Array[String]) {

   val happy = io.Source.fromFile(args(0)+"/happy.txt").getLines.toList.map{x=> val spl=x.split("\t"); Array("positive", spl(2))}
   val sad = io.Source.fromFile(args(0)+"/sad.txt").getLines.toList.map{x=> val spl=x.split("\t"); Array("negative", spl(2))}
   val neut = io.Source.fromFile(args(0)+"/neutral.txt").getLines.toList.map{x=> val spl=x.split("\t"); Array("neutral", spl(2))}

   val all = happy ++ sad ++ neut

   val xml = <dataset>{all.map(x => 
                <item label={ x(0) } tweetid="unknown" date="unknown" target="unknown" username="unknown">
                  <content>{ x(1) }</content>
                </item>
              )}</dataset>

  println(xml)
 }


}