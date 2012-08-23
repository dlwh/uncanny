package dlwh.uncanny.lj

import java.net.URL

/**
 *
 * @author dlwh
 */
case class Event(url: URL, user: String, postId: Int, eventType: String)


trait EventTemplate {
  // examine all files that match query (coarse pass)
  def query: String
  // fine pass with an actual regex
  def regex: String
  def eventType: String
}

object EventTemplates {

  val templates = Seq(GotSick)

  object GotSick extends EventTemplate {
    def query: String = "sick"
    def regex: String = "(felt|got|feel) sick"

    def eventType = "GotSick"
  }

  object FailedTest extends EventTemplate {
    def query: String = "failed"
    def regex: String = "failed (exam|test)"

    def eventType = "FailedTest"
  }

  object Graduated extends EventTemplate {
    def query: String = "graduated"

    // fine pass with an actual regex
    def regex: String = "I .* graduate"

    def eventType = "Graduated"
  }
}