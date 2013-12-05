package com.sysiq.common.parsers.tests

import com.sysiq.common.parsers._
object TestParser {

  def main(args: Array[String]): Unit = {
    TestParsers.normalizeUrl("/Stores/WebContent/AuroraStorefrontAssetStore/UserArea/ServiceSection/InterestItemListSubsection/WishListDisplay.jsp","/${sdb.jspStoreDir}/include/BreadCrumbTrailDisplay.jsp")
  }

}