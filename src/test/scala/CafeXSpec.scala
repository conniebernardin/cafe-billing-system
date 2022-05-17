import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CafeXSpec extends AnyWordSpec with Matchers {

    "loyaltyDiscount" should {
      "not apply a discount for 0 stars" in {
        1 mustBe 2
      }
    }

}
