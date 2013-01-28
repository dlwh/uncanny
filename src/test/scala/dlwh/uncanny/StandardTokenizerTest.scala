package dlwh.uncanny

import org.scalatest.prop.Checkers
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */

class StandardTokenizerTest  extends FunSuite with Checkers {

  private def isOneToken(w: String) =
    w === StandardTokenizer(w).head

  test("simple words") {
    val words = List("Hi","there","pilgrim","happy","Thanksgiving","there")
    for(w <- words) {
      assert(isOneToken(w))
    }
  }

  test("some symbols") {
    val words = List(".","...","$","-","/")
    for(w <- words) {
      assert(isOneToken(w))
    }
    val special = Map(
     "(" -> "-LRB-",
     ")" -> "-RRB-",
     "[" -> "-LSB-",
     "]" -> "-RSB-",
     "{" -> "-LCB-",
     "}" -> "-RCB-"
    )

    for( (s,t) <- special) {
      assert(StandardTokenizer(s).toList === List(t))
    }
  }

  test("simple sentences") {
    val sents = Map( "Every good boy does fine." -> List("Every","good","boy","does","fine","."),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim?" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","?"),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim!" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","!"),
    "Hi there, (pilgrim); happy Thanksgiving there, pilgrim!" -> List("Hi","there",",","-LRB-", "pilgrim", "-RRB-", ";","happy","Thanksgiving","there",",","pilgrim","!")
    )
    for( (s,toks) <- sents) {
      assert(StandardTokenizer(s).toList === toks)
    }
  }

  test("quotes") {
    val sents = Map("\"Hi there\"" -> List("``","Hi","there","''"),
      "\"Hi there.\"" -> List("``","Hi","there",".","''"))
    for( (s,toks) <- sents) {
      assert(StandardTokenizer(s).toList === toks)
    }
  }

  test("contractions") {
    val sents = Map(//"didn't" -> List("did","n't"),
      "ya'll" -> List("ya","'ll"),
      "we're" -> List("we","'re"),
      "we've" -> List("we","'ve"),
      "YA'LL" -> List("YA","'LL"),
      "WE'RE" -> List("WE","'RE"),
      "WE'VE" -> List("WE","'VE"),
      "cannot" -> List("can","not"),
      "can't" -> List("ca","n't"),
      "CAN'T" -> List("CA","N'T"),
      "I'm" -> List("I","'m"),
      "He's" -> List("He","'s"),
      "parents'" -> List("parents","'")
    )
    for( (s,toks) <- sents) {
      assert(StandardTokenizer(s).toList === toks)
    }
  }

  test("moneys") {
    assert(StandardTokenizer("99").toList === List("99"))
    assert(StandardTokenizer("$99").toList === List("$","99"))
    assert(StandardTokenizer("$99.33").toList === List("$","99.33"))
  }

  test("special words") {
    val words = Map("cannot" -> List("can","not"),
        "d'ye"-> List("d'","ye"),
        "gimme"->List("gim","me"),
        "gonna"->List("gon","na"),
        "gotta"->List("got","ta"),
        "Lemme"->List("Lem","me"),
        "more'n"->List("more","'n"),
        "'tis"->List("'t","is"),
        "'Tis"->List("'T","is"),
        "wanna"->List("wan","na")
//        "Whaddya"->List("Wha","dd","ya"),
//        "Whatcha"->List("Wha","t","cha")
    )
    for( (s,toks) <- words) {
      assert(StandardTokenizer(s).toList === toks)
    }

  }

  test("acronyms") {
    val candidates = Seq("U.S.","u.s.","p.s.")
    for(s <- candidates) {
      assert(StandardTokenizer(s).toList === List(s,"."))
    }
  }

  test("URLs") {
    val text = "Go to http://google.com/ now!"
    assert(StandardTokenizer(text).toList === List("Go", "to", "http://google.com/", "now", "!"))
  }
}
