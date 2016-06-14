package cart

import scala.util.Random

trait ItemCode {
  def id: String
}

object CartSystem {
  type Currency = Int
  
  def apply() = new CartSystem;
}

trait Priceable {
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
    
    private[cart] case object Apple extends Priceable {
      val itemCode = AppleCode
      val unitPrice = 60
    }
    private[cart] case object Orange extends Priceable {
      val itemCode = OrangeCode
      val unitPrice = 25
    }
    
    private val items = Map[ItemCode,Priceable](Apple.itemCode -> Apple, Orange.itemCode -> Orange)
    
    def lookup(item: ItemCode) = items get item
    def available: Seq[ItemCode] = items.map( _._1).toSeq
  }
  
  class Session private(val items: List[Priceable]) {
    
    def this() = this(Nil)
    
    def addItem(item: ItemCode): Session = {
      Registry.lookup(item) match {
        case Some(i) => new Session( i :: items )  
        case None => throw new IllegalArgumentException(s"Wrong item code ${item}")
      }
    }
  }
  
  private object Billing {
    def apply() = new Billing
  }
  
  private class Billing private(private val items: Map[Priceable, Int]) {
        private def this() = this(Map())  
    
    def appendItem(item: Priceable): Billing = new Billing( items.updated(item, items.getOrElse(item, 0) + 1) )
    def total: CartSystem.Currency = {
          items.foldLeft(0)( (sum,p) => p match {
            // the below can be reduced to a single formula "sum + ( (num / divisor) * (divisor-1) + (num%divisor))* unitPrice
            // but that wouldn't scale much
              case (Registry.Apple, num) => sum + ((num / 2).toInt + (num & 1)) * Registry.Apple.unitPrice  
              case (Registry.Orange, num) => sum + ((num / 3).toInt * 2 + (num % 3)) * Registry.Orange.unitPrice 
          })
        }
  }
  
  def session = new Session
  def checkout(s: Session): CartSystem.Currency = {
    val billing = s.items.foldLeft( Billing() )((b,item)=> b.appendItem(item))
    billing.total
  }
  
}
// to minimize deps and complexity no juint
object CartTest extends App {
  
  def test1 = {
    
    val cart = CartSystem()
    def doTst(items: Seq[ItemCode] ) {
      
      val sess1 = cart.session
      assert(0 == cart.checkout(sess1))
      
      val f = ( p: (Map[Priceable, Int], cart.Session) ) => {
          val it1 = cart.Registry.available( Random.nextInt( cart.Registry.available.size  ) )

        }
      
      val (itemMap, sess) = items.foldLeft( (Map[Priceable, Int](), sess1) )((p, item)=> {
          val found = cart.Registry.lookup(item)
          assert( found.isDefined )
          (p._1.updated(found.get, 1 + p._1.getOrElse(found.get, 0) ), p._2.addItem(item))
      })
      
      val apples = itemMap.getOrElse(cart.Registry.Apple, 0)
      val oranges = itemMap.getOrElse(cart.Registry.Orange, 0)
      val sum = ((apples / 2).toInt + (apples & 1)) * cart.Registry.Apple.unitPrice + 
                ((oranges / 3).toInt * 2 + (oranges % 3)) * cart.Registry.Orange.unitPrice 
      assert(sum == cart.checkout(sess)) 
    }
    
    for (apples <- 0 to 11) {
      for (oranges <- 0 to 11) {
        doTst( Random.shuffle(Iterator.range(0, apples).map(_=> cart.Registry.AppleCode ).toSeq ++  
                Iterator.range(0, oranges).map(_=> cart.Registry.OrangeCode ) ))
      }
    }
    println("Tests OK")  
  }
  
  def negative {
    val res = try {
      CartSystem().session.addItem( new ItemCode { val id = "plum" } )
      false
    } catch {
      case e:IllegalArgumentException => true
      case _:Throwable => false
    }
    assert(res)
  }
  
  test1
  negative
  
}
