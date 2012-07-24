package dlwh.uncanny

import breeze.text.tokenize.Tokenizer
import java.io.StringReader
import breeze.util.Iterators

/**
 *
 * @author dlwh
 */

case class StandardTokenizer() extends Tokenizer {
  def apply(text: String):Iterable[String] = {
    new Iterable[String]() {
      def iterator: Iterator[String] = {
        val impl = new StandardTokenizerImpl(new StringReader(text + "\n"))
        Iterators.fromProducer{
          try {
            Option(impl.getNextToken())
          } catch {
            case e: Throwable => throw new RuntimeException("Could not tokenize " + text, e)
          }
        }.takeWhile(_ != null)
      }
    }

  }

}

object StandardTokenizer extends StandardTokenizer
