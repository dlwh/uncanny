package dlwh.uncanny.lj

import scalala.tensor.mutable.Counter2
import java.io.File

/**
 *
 * @author dlwh
 */

object BuildGraph extends App {
  val counts = Counter2[Int, Int, Double]()
  for(file <- args.map(new File(_)).iterator; ex <- scalanlp.util.readObject[Array[Array[Int]]](file)) {
    for( (next,prev) <- ex.drop(1).zip(ex))  {
      counts(next, prev) += 1
    }
  }

  val totals = scalala.library.Library.sum(counts)
  val sum = totals.sum

  val pruned = collection.mutable.Set[Int]()

  println("digraph G { ")
  for( (k,v) <- totals.pairsIterator) {
    val shape = Moods.coarseMoodArray(k) match {
      case 1 => "diamond"
      case 2 => "circle"
      case _ => "square"
    }

    val color = Moods.coarseMoodArray(k) match {
      case 1 => "#00FF00"
      case 2 => "#0000FF"
      case _ => "#FFFFFF"

    }

    val width = (v / sum * 100)
    if(width > .9 && Moods.coarseMoodArray(k) <= 2)
      println("\"" + Moods.moods.get(k) + "\" [shape=\""+shape + "\", color=\"" + color +"\", style=filled, penwidth="+(width/2) + "];")
    else
      pruned += k
  }


  for( (prev, next, v) <- counts.triplesIterator if !pruned(prev) && !pruned(next)) {
    val width = (v / totals(prev) * 100)
    if(width >= 2.5)
      println("\"" + Moods.moods.get(prev) + "\" -> \"" + Moods.moods.get(next) +"\" [penwidth="+ (width)+ "];")
  }

  println("}")


}
