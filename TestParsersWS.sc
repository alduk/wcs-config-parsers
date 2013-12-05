object TestParsersWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val s = "${env_jspStoreDir}Widgets/CatalogEntry/CatalogEntryDisplay.jsp"
                                                  //> s  : String = ${env_jspStoreDir}Widgets/CatalogEntry/CatalogEntryDisplay.jsp
                                                  //| 
  TestParsers.normalizeUrl("/Stores/WebContent/AuroraStorefrontAssetStore/Widgets/LeftNavigation/LeftNavigation_Data.jspf","FacetFilter.jspf")
                                                  //> res0: String = /Stores/WebContent/AuroraStorefrontAssetStore/Widgets/LeftNav
                                                  //| igation/FacetFilter.jspf
  s.replaceAll("\\$\\{env_jspStoreDir\\}", "/Stores/WebContent/AuroraStorefrontAssetStore/")
                                                  //> res1: String = /Stores/WebContent/AuroraStorefrontAssetStore/Widgets/Catalog
                                                  //| Entry/CatalogEntryDisplay.jsp
}