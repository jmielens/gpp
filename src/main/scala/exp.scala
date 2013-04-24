package gpp.classify
import scala.xml._
import nak.util._
import chalk.lang.eng._
import nak.NakContext._
import nak.core._
import nak.NakContext
import nak.data._
import nak.liblinear.LiblinearConfig
import nak.util.ConfusionMatrix
import chalk.lang.eng.PorterStemmer

object Classify {

    def main(args: Array[String]): Unit = {
        val opts = ClassifyOpts(args)

        if (opts.method() == "majority") {
            majorityClassify(opts.trainfile(),opts.evalfile(),opts.detail())
        } else if (opts.method() == "lexicon") {
            lexiconClassify(opts.evalfile(),opts.detail())
        } else {
            liblinearClassify(opts.trainfile(), opts.evalfile(), opts.detail(), opts.cost(), opts.ext())
        }
    }

    def majorityClassify(train: List[String], eval: List[String], detail: Boolean): Unit = {
        // Read Training Tweets
        val (tLabels, tTargets, tTweets) = readXmlTweets(train)

        // Count labels to find Majority Label
        val majorLabel = tLabels.groupBy{identity}
                                .mapValues(_.size).toList
                                .sortBy(_._2).reverse.head._1

        // Read Eval tweets
        val (eLabels, eTargets, eTweets) = readXmlTweets(eval)

        // 'Classify' the eval tweets by calling them all the majority label
        val predLabels = Array.fill[String](eLabels.size)(majorLabel)

        // Confusion...
        val c = ConfusionMatrix(eLabels.toSeq,predLabels.toSeq,eTweets.toSeq)
        println(c)
        if (detail) {
            println(c.detailedOutput)
        }
    }

    def lexiconClassify(eval: List[String], detail: Boolean): Unit = {
        // Read Eval tweets
        val (eLabels, eTargets, eTweets) = readXmlTweets(eval)

        val pos = scala.io.Source.fromFile("data/sentiment/positive-words.txt").getLines.toSet
        val neg = scala.io.Source.fromFile("data/sentiment/negative-words.txt").getLines.toSet

        val predLabels = eTweets.map{tweet =>
            val toked = Twokenize.tokenize(tweet)
            val posCount  = toked.map(x => pos.contains(x)).count(x => x == true)
            val negCount  = toked.map(x => neg.contains(x)).count(x => x == true)
            val neutCount = toked.map(x => !pos.contains(x) && !neg.contains(x)).count(x=>x==true)/25

            List((posCount,"positive"),(negCount,"negative"),(neutCount,"neutral")).max._2
        }

        // Confusion...
        val c = ConfusionMatrix(eLabels.toSeq,predLabels.toSeq,eTweets.toSeq)
        println(c)
        if (detail) {
            println(c.detailedOutput)
        }
    }

    def liblinearClassify(train: List[String], eval: List[String], detail: Boolean, costVal: Double, extended: Boolean): Unit = {
        val (tLabels, tTargets, tTweets) = readXmlTweets(train)
        val (eLabels, eTargets, eTweets) = readXmlTweets(eval)
        val toked = eTweets.map(x => Twokenize.tokenize(x))
        val stopwords = io.Source.fromFile("data/stopwords.txt").getLines.toSet

        val pos = scala.io.Source.fromFile("data/sentiment/positive-words.txt").getLines.toSet
        val neg = scala.io.Source.fromFile("data/sentiment/negative-words.txt").getLines.toSet
        val stemmer = new PorterStemmer
    
        val labeledTweets = tLabels.zip(tTweets)
        val trainingExamples = for(labelTweetPair <- labeledTweets)
            yield{
                Example(labelTweetPair._1, labelTweetPair._2)
            }

        // Set-up featurizer & cost values
        val config = LiblinearConfig(cost=costVal)
        val featurizer = if (extended) {
                new Featurizer[String,String] {
                    def apply(in: String) = {
                        val toked = Twokenize.tokenize(in)
                        val goodWords = toked.filterNot(stopwords) // Filter Stopwords

                        val wordFeatures = goodWords.map(word => ("word",word.toLowerCase))
                        val sentFeatures = goodWords.map{word =>
                            if(pos.contains(word)){
                                ("sentiment","pos")
                            } else if (neg.contains(word)){
                                ("sentiment","neg")
                            } else {
                                ("sentiment","neut")
                            }
                        }
                        val stemFeatures = goodWords.map(word => ("stem",stemmer(word)))


                        val allFeatures = wordFeatures ++ sentFeatures
                        for (featurePair <- allFeatures)
                            yield FeatureObservation(featurePair._1+"="+featurePair._2)
                    }
                }
            } else {
                new Featurizer[String,String] {
                    def apply(in: String) = {
                        val toked = Twokenize.tokenize(in)
                        val goodWords = toked.filterNot(stopwords) // Filter Stopwords
                        for (word <- goodWords)
                            yield FeatureObservation("word"+"="+word)
                    }
                }
            }

        val classifier = NakContext.trainClassifier(config, featurizer, trainingExamples)
    
        println("Evaluating... " +eval)

        val testLabeledTweets = eLabels.zip(eTweets)    
        val maxLabeltweets = NakContext.maxLabel(classifier.labels) _
        val comparisons = for (ex <- testLabeledTweets) yield
            (ex._1, maxLabeltweets(classifier.evalRaw(ex._2)), ex._2)
        val (gold, pred, inputs) = comparisons.unzip3

        // Confusion...
        val c = ConfusionMatrix(gold,pred,inputs)
        println(c)
        if (detail) {
            println(c.detailedOutput)
        }
    }

    def readXmlTweets(file: List[String]) = {

        val xRoot: NodeSeq = file.map{f =>
            scala.xml.XML.load(scala.xml.Source.fromFile(f)) toSeq
        }.reduceLeft(_++_)

        val labels  = (xRoot \ "item") map {x => (x \ "@label").text} toList
        val targets = (xRoot \ "item") map {x => (x \ "@target").text} toList
        val tweets  = (xRoot \\ "content") map {x => x.text} toList

        val filtered = labels.zip(targets).zip(tweets).filter(x => x._1._1 == "positive" || x._1._1 == "negative" || x._1._1 == "neutral")
        val (tmp, filteredTweets) = filtered.unzip
        val (filteredLabels, filteredTargets) = tmp.unzip

        (filteredLabels, filteredTargets, filteredTweets)
    }
}

object ClassifyOpts {
    import org.rogach.scallop._

    def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification Application.

For usage see below
""")
    val methods = Set("majority","L2R_LR","lexicon")

    val trainfile = opt[List[String]]("train", short = 't',required=false,descr="The files containing training events.")
    val evalfile = opt[List[String]]("eval", short='e', descr="The files containing evalualation events.")
    val method = opt[String]("method", short='m', default=Some("L2R_LR"), validate = methods, descr = "The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type.")
    val cost = opt[Double]("cost", short='c',default=Some(1.0), required=false, descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")
    val ext = opt[Boolean]("extended", short='x', required=false, descr="Use extended features.")
    val verbose = opt[Boolean]("verbose",short='v')
    val detail = opt[Boolean]("detailed", short='d', required=false)
  }
}