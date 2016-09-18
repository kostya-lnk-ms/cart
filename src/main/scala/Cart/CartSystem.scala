package Cart

import scala.util.{Failure, Success, Try}

trait ItemCode {
  def id: String
}

sealed trait Priceable {
  def itemCode: ItemCode
  def unitPrice: CartSystem.Currency
}

object CartSystem {
  type Currency = BigDecimal
  val currencyOps = implicitly[ Numeric[Currency] ]
  def apply() = new CartSystem
}

class CartSystem {
  
  // Stab object, the real inventory would be external
  private[Cart] object Registry {
    private[Cart] case object CodeA extends ItemCode {
      val id = "A"
    }
    private[Cart] case object CodeB extends ItemCode {
      val id = "B"
    }
    private[Cart] case object CodeC extends ItemCode {
      val id = "C"
    }
    private[Cart] case object CodeD extends ItemCode {
      val id = "D"
    }
    
    private[Cart] case object ItemA extends Priceable {
      val itemCode = CodeA
      val unitPrice: CartSystem.Currency = 50
    }
    private[Cart] case object ItemB extends Priceable {
      val itemCode = CodeB
      val unitPrice: CartSystem.Currency = 30
    }
    private[Cart] case object ItemC extends Priceable {
      val itemCode = CodeC
      val unitPrice: CartSystem.Currency = 20
    }
    private[Cart] case object ItemD extends Priceable {
      val itemCode = CodeD
      val unitPrice: CartSystem.Currency = 15
    }

    private[Cart] val available = Seq(ItemA, ItemB, ItemC, ItemD)

    private val items:Map[ItemCode, Priceable] = available.map(i=> (i.itemCode, i) ).toMap

    def lookup(item: ItemCode) = items get item
  }

  object Session {
    def apply() = new Session(Nil)
  }

  class Session private(val items: Seq[Priceable]) {
    
    def addItem(item: ItemCode): Try[Session] = {
      Registry.lookup(item) match {
        case Some(i) => Success( new Session( items :+ i ) )
        case None => Failure(new IllegalArgumentException(s"Wrong item code ${item}"))
      }
    }
  }

  private object Billing {
    def apply() = new Billing(Nil)
  }
  
  private class Billing private(private val items: Seq[Priceable]) {
    import CartSystem._

    private trait BillingRule {
      def process(priceable: Priceable): BillingRule
      def total : Currency
      def combine(other: BillingRule) = {
      }
    }

    private object SumRule {

      private[this] class Impl(override val total: Currency) extends BillingRule {
        override def process(priceable: Priceable) = new Impl(total + priceable.unitPrice)
      }
      def apply(): BillingRule = new Impl(0)
    }

    private object DiscountRule {

      private[this] class Impl(override val total: Currency, count : Int, factor: Int, code: ItemCode ) extends BillingRule {
        override def process(priceable: Priceable) = {
          if (code == priceable.itemCode) {
            val (newCount, newSum ) = if (count + 1 == factor) (0, total - priceable.unitPrice) else (count + 1, total)
            new Impl(newSum, newCount, factor, code)
          }
          else
            this
        }
      }

      def apply(code: ItemCode, factor: Int): BillingRule = new Impl(0, 0, factor, code)
    }

    private val rules = Seq(SumRule(), DiscountRule(Registry.CodeA, 2) )

    def appendItem(item: Priceable): Billing = new Billing( items :+ item )

    def total: Currency = {

      rules.foldLeft( currencyOps.zero )((total, rule)=>
        total + items.foldLeft( rule )((r, item)=> r.process(item) ).total
      )
    }
  }
  
  def session = Session()
  def checkout(s: Session): CartSystem.Currency = {
    val billing = s.items.foldLeft( Billing() )((billing, item)=> billing.appendItem(item))
    billing.total
  }
  
}
