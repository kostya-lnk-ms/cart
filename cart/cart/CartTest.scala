package cart

import scala.util.Random

trait ItemCode {
  def id: String
}

object CartSystem {
  type Currency = Int
  
  def apply() = new CartSystem;
}

trait Pricaeble {
  def itemCode: ItemCode
  def unitPrice: CartSystem.Currency
}


class CartSystem {
  
  object Registry {
    private[cart] case object AppleCode extends ItemCode {
      val id = "apple"
    }
    private[cart] case object OrangeCode extends ItemCode {
      val id = "orange"
    }
    
    private[cart] case object Apple extends Pricaeble {
      val itemCode = AppleCode
      val unitPrice = 60
    }
    private[cart] case object Orange extends Pricaeble {
      val itemCode = OrangeCode
      val unitPrice = 25
    }
    
    private val items = Map[ItemCode,Pricaeble](Apple.itemCode -> Apple, Orange.itemCode -> Orange)
    
    def lookup(item: ItemCode) = items get item
    def availalble: Seq[ItemCode] = items.map( _._1).toSeq
  }
  
  class Session private(val items: List[Pricaeble]) {
    
    def this() = this(Nil)
    
    def addItem(item: ItemCode): Session = {
      Registry.lookup(item) match {
        case Some(i) => new Session( i :: items )  
        case None => throw new IllegalArgumentException(s"Wrong item code ${item}")
      }
    }
  }
  
  def session = new Session
  def checkout(s: Session): CartSystem.Currency = {
    s.items.map { _.unitPrice } sum
  }
  
}

object CartTest extends App {
  
  def test1 = {
    
    val cart = CartSystem()
    
    var sess1 = cart.session
    assert(0 == cart.checkout(sess1))
    
    var sess2 = cart.session
    assert(0 == cart.checkout(sess2))

    val f = ( p: (CartSystem.Currency, cart.Session) ) => {
        val it1 = cart.Registry.availalble( Random.nextInt( cart.Registry.availalble.size  ) )
        val found = cart.Registry.lookup(it1)
        assert( found.isDefined )
        (p._1 + found.get.unitPrice, p._2.addItem(it1))
      }
    
    val res = Iterator.range(0, 5).foldLeft( ((0, sess1), (0, sess2) ))((p, _)=> ( f(p._1), f(p._2) ) )
    
    Seq(res._1, res._2).foreach {p => assert(p._1 == cart.checkout(p._2)) }
  }
  
  test1
}