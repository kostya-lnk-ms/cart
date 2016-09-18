package Cart

import org.junit.Test
import org.junit.Assert._
import org.scalacheck.Prop.True

import scala.util.Try

class CartTest {

  import CartSystem.Currency

  @Test
  def testSimple {

    val cart = CartSystem()

    val sess1 = cart.session
    assertTrue(cart.checkout(sess1) == 0)

    val sess2 = cart.session
    assertTrue(cart.checkout(sess2) == 0 )
  }

  @Test
  def testSimpleTotals {

    val cart = CartSystem()
    import cart.Registry._

    val scenarios = Seq[(Currency, Seq[ItemCode])] (
                        (0, Nil),
                        (50, Seq(CodeA)),
                        (80, Seq(CodeA, CodeB)),
                        (115, Seq(CodeC, CodeD, CodeB, CodeA)),
                        (40, (1 to 2).map(_=> CodeC)),
                        (85, (1 to 2).map(_=> CodeC) ++ (1 to 3).map(_=> CodeD)),
                        (110,(1 to 2).map(_=> CodeA) ++ (1 to 2).map(_=> CodeB)),
                        (160,(1 to 3).map(_=> CodeA) ++ (1 to 2).map(_=> CodeB))
                    )

    scenarios foreach {
      case (sum, items) =>
        val sess = items.foldLeft( Try( cart.session) )((s, it) => s.flatMap( _.addItem(it) ) )
        assertTrue(sess.isSuccess)
        assertEquals(sum, cart.checkout(sess.get))
    }
  }

  @Test(expected = classOf[IllegalArgumentException])
  def negativeTest {
    CartSystem().session.addItem(new ItemCode {
      val id = "plum"
    }).get
  }



}
