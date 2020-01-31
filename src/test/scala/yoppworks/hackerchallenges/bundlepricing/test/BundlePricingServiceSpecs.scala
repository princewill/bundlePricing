package yoppworks.hackerchallenges.bundlepricing.test

import org.scalatest._
import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions._
import yoppworks.hackerchallenges.bundlepricing.BundlePricingService

import scala.util.{Failure, Success}

class BundlePricingServiceSpecs extends FlatSpec with Matchers {

  // build a test state ( catalog, bundles )

  val appleCatalogItem = CatalogItem("Apple", Price(199))
  val margarineCatalogItem = CatalogItem("Margarine", Price(250))
  val breadCatalogItem = CatalogItem("Bread", Price(300))

  val catalogExample = Seq(appleCatalogItem, margarineCatalogItem, breadCatalogItem)

  val currentBundles = Seq(
    // 1 apple 1.99 , 2 apples 2.15
    BundleTotalPriceDiscount(
      Seq(CartItem(appleCatalogItem, Quantity(2))),
      totalPrice = Price(215)
    ),
    // 1 bread + 2 margarines, the 2nd margarine is free
    BundleDiscountOnItemUnitPrice(
      Seq(
        MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        // 2nd margarine Free!
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = Some(Price(0)))
      )
    )
  )

  // ps = pricingService
  val ps = new BundlePricingService(catalogExample, currentBundles)

  // tests

  "A Bundle Pricing Service" should "find the lowest possible price ; Simple bundle case : 1 apple 1.99 , 2 apples 2.15 " in {

    // 1 apple , no bundle
    ps.bundleCartToLowestPrice(Cart(Seq(
       CartItem(appleCatalogItem, Quantity(1))
     ))) should be (Success(Price(199)))

    // 2 apple, bundle !
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(2))
    ))) should be (Success(Price(215)))

    // 3 apples, we are allowed to use 2 bundles of 2 apples because items can be reused
    // However in this case  2 apples bundle + 1 apple alone is cheaper than 2 bundles
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(3))
    ))) should be (Success(Price(215+199)))

    // 4 apples, should be grouped into 2 (2 apples) bundles
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(4))
    ))) should be (Success(Price(215+215)))

    // 5 apples, should be grouped into 2 (2 apples) bundles + 1 apple one (lowest price than 3 (2Apples) bundles
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(5))
    ))) should be (Success(Price(215+215+199)))

  }

  it should "find the lowest possible price ; More complex bundle case : 1 bread + 2 magarines, the 2nd margarine is free " in {

    // 1 bread , 1 margarine, no bundle just the sum of unit prices
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(1))
    ))) should be (Success(Price(300+250)))

    // 1 bread , 2 margarines, bundle, 2nd margarine is free
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(2))
    ))) should be (Success(Price(300+250)))

    // 1 bread , 3 margarines, bundle, we still should have the 2nd margarine free, + pay for the 3rd
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(3))
    ))) should be (Success(Price(300+250+250)))

  }

  it should "find the lowest possible price ; Complex cart where multiple bundle types can be found " in {

    // a small shopping list with various products, but no bundle
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(1)),
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(1))
    ))) should be (Success(Price(199+300+250)))

    // a small shopping list with various products,
    // (2 apples) bundle
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(3)),
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(1))
    ))) should be (Success(Price(215+199+300+250)))

    // a small shopping list with various products,
    // (2 apples) bundle + 1 bread, 2 margarines bundle
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(3)),
      CartItem(breadCatalogItem, Quantity(1)),
      CartItem(margarineCatalogItem, Quantity(2))
    ))) should be (Success(Price(215+199+300+250)))

    // a small shopping list with various products,
    // (2 apples) bundle + 1 bread, 2 margarines bundle, 1 additional single bread alone
    ps.bundleCartToLowestPrice(Cart(Seq(
      CartItem(appleCatalogItem, Quantity(3)),
      CartItem(breadCatalogItem, Quantity(2)),
      CartItem(margarineCatalogItem, Quantity(2))
    ))) should be (Success(Price(215+199+300+250+300)))

  }


  it should "Fail with InvalidCartException if consumer pass a Cart with invalid catalog item(s)" in {
    ps.bundleCartToLowestPrice(
      Cart(Seq(
        CartItem(CatalogItem("Pink Apple", Price(300)), Quantity(10))
        // unfortunately Pink apple is no longer in catalog
      ))
    ) should be (Failure(InvalidCartException))
  }

  it should "be able to detect invalid cart" in {
    ps.isCartValid(
      Cart(Seq(
        CartItem(CatalogItem("Pink Apple", Price(300)), Quantity(10))
        // unfortunately Pink apple is no longer in catalog
      ))
    ) should be (false)
  }

}

