package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions.{BundleDiscountOnItemUnitPrice, BundleTotalPriceDiscount}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Specs for the BundlePricingService, which take as parameter a catalog and the current promotions
  * and then can bundle a cart to optimize the price
  */
class BundlePricingService(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {

  /**
    * Group cart item to bundles to get the lowest possible cart price
    * @return
    *   Success: cart price in cents, example Price(2250) => $22.50
    *   Failure: InvalidCartException if the cart isn't valid (contains an item which doesn't exist in catalog)
    */
  def bundleCartToLowestPrice(cart: Cart) =
			if(!isCartValid(cart)) Future.failed(InvalidCartException)
			else loop(cart)


  private def loop(initCart: Cart): Future[Price] = {
			val lowestCartPrice = bundlePromotions.map { bundle =>
				applyBundleToCart(initCart, bundle) match {
					case Some(unbundledRestOfCart) => loop(unbundledRestOfCart).map(_.value + bundle.totalDiscountedPrice.value)
					case None => Future.successful(initCart.cartItems.map(_.cartItemTotalPrice).sum)
				}}
			Future
				.sequence(lowestCartPrice)
				.map(_.reduceLeft{(accumulator, price) => if (price < accumulator) price else accumulator }).map(Price(_))
		}

  private def applyBundleToCart(cart: Cart, bundlePromotion: BundlePromotion): Option[Cart] = bundlePromotion match {
      case bundle: BundleTotalPriceDiscount => BundlePricingService.handleBundleTotalPriceDiscount(cart, bundle)
      case bundle: BundleDiscountOnItemUnitPrice => BundlePricingService.handleBundleDiscountOnItemUnitPrice(cart, bundle)
    }

  def isCartValid(cart: Cart): Boolean = {
    val invalid = cart.cartItems.map(_.catalogItem.name).diff(catalog.map(_.name))

    if (invalid.nonEmpty) {
      InvalidCartException
      false
    }
    else if (cart.cartItems.isEmpty) {
      InvalidCartException
      false
    }
    else true
  }

}

object BundlePricingService {

  def handleBundleTotalPriceDiscount(cart: Cart, bundle: BundlePromotion): Option[Cart] = {
    val listBuffer = ListBuffer.empty[CartItem]
    val cartItems = cart.cartItems
    val (bundleCatalogItem, bundleCatalogItemQty)  = bundle.cartItems.map(item => item.catalogItem -> item.quantity.value).head
    val nonBundleCartItems = cartItems.filterNot(_.catalogItem == bundleCatalogItem)

    if (isBundleable(cartItems, (bundleCatalogItem, bundleCatalogItemQty))) {
      val unbundled = cartItems.flatMap{ cartItem =>
        if(cartItem.quantity.value > bundleCatalogItemQty)
          listBuffer :+ CartItem(cartItem.catalogItem, Quantity(cartItem.quantity.value - bundleCatalogItemQty))
        else listBuffer
      }
      Some(Cart(unbundled ++ nonBundleCartItems))
    }
    else None

  }

  def handleBundleDiscountOnItemUnitPrice(cart: Cart, bundle: BundlePromotion): Option[Cart] = {
    val listBuffer = ListBuffer.empty[CartItem]
    val cartItems = cart.cartItems
    val bundleCatalogItemsMap = bundle.cartItems.map(item => (item.catalogItem, item.quantity.value)).toMap
    val isEligibleForBundle = bundleCatalogItemsMap.map{ case (bundleCatalogItem: CatalogItem, bundleQuantity: Int) =>
      isBundleable(cartItems, (bundleCatalogItem, bundleQuantity))}.forall(_ == true)
    val nonBundleItems = cartItems.filterNot(cartItem => bundleCatalogItemsMap.get(cartItem.catalogItem).isDefined)
    val bundleItems = cartItems.filter(cartItem => bundleCatalogItemsMap.get(cartItem.catalogItem).isDefined)

    if(isEligibleForBundle) {
      val unbundled = bundleItems.flatMap { item =>
        val catalogItem = item.catalogItem
        val itemQty = item.quantity.value
        val discountedQty = bundleCatalogItemsMap(catalogItem)

        if (itemQty > discountedQty) listBuffer :+ CartItem(catalogItem, Quantity(itemQty - discountedQty))
        else listBuffer
      }
      Some(Cart(unbundled ++ nonBundleItems))
    }
      else None
  }


  def isBundleable(cartItems: Seq[CartItem], bundles: (CatalogItem, Int)): Boolean =
    cartItems.exists(cartItem => cartItem.catalogItem == bundles._1 && cartItem.quantity.value >= bundles._2)
}