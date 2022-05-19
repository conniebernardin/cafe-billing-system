import Currency.{EUR, GBP}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class CafeXSpec extends AnyWordSpec with Matchers {


  val coffee = MenuItem("Cafe", 1.00, isHot = true, isFood = false, isPremium = false)
  val whiteWine = MenuItem("Sauvignon Blanc", 4.00, isHot = false, isFood = false, isPremium = true)

  val cheeseSandwich = MenuItem("Cheese Sandwich", 2.00, isHot = false, isFood = true, isPremium = false)
  val steakFrites = MenuItem("Steak frites", 4.50, isHot = true, isFood = true, isPremium = false)
  val ratatouille = MenuItem("Ratatouille", 14.00, isHot = true, isFood = true, isPremium = false)

  val lobster = MenuItem("Lobster", 25.00, isHot = true, isFood = true, isPremium = true)

  case object CAD extends Currency

  "checkIfOrderIsOnlyDrinks" should {
    "return false if order contains food" in {
      val order: List[MenuItem] = List(whiteWine, cheeseSandwich )

      assert(CafeX.checkIfOrderIsOnlyDrinks(order).equals(false))

    }
  }

  "checkIfOrderIsOnlyDrinks" should {
    "return true if order only contains drinks" in {
      val order: List[MenuItem] = List(whiteWine, coffee )

      assert(CafeX.checkIfOrderIsOnlyDrinks(order).equals(true))

    }
  }

  "currencyExchangeRate" should {
    "return 1.18 rate when Euro input" in {
      assert(CafeX.currencyExchangeRate(EUR).equals(1.18))
    }
  }

  "currencyExchangeRate" should {
    "return x1 rate when unknown input" in {
      assert(CafeX.currencyExchangeRate(CAD).equals(1))
    }
  }

  "setCurrencySymbol" should {
    "return pound sign as string when GBP input as currency" in {
      assert(CafeX.setCurrencySymbol(GBP).equals("£"))
    }
  }

  "setCurrencySymbol" should {
    "return default pound sign as string when unknown input as currency" in {
      assert(CafeX.setCurrencySymbol(CAD).equals("£"))
    }
  }

}



