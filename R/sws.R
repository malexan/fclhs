### Functions to work with SWS

#' Get Comptrade trading data from SWS.
#' 
#' @import dplyr
#' @import faosws
#' @export
getswsct <- function(area, year, partner #, 
                     #flow = "all", code = "AG6" #, compact = T,
                     #desc = F, debug = F
                     ) {
  
  dmn    <- "trade"
  dtset  <- "ct_raw_tf"
  rtvar  <- "reportingCountryM49"
  ptvar  <- "partnerCountryM49"
  itmvar <- "measuredItemHS"
  elevar <- "measuredElementTrade"
  yrvar  <- "timePointYears"
  
  if(missing(partner)) {
    partner <- GetCodeList(dmn, dtset, dimension = ptvar)$code
  }
  
  dms <- list(Dimension(name = rtvar,
                        keys = area),
              Dimension(name = ptvar,
                        keys = partner),
              Dimension(name = itmvar,
                        keys = "010110"),
              Dimension(name = elevar,
                        keys = c("5621", "5921")),
              Dimension(name = yrvar, 
                        keys = year))
  
  k <- DatasetKey(domain     = dmn,
                  dataset    = dtset,
                  dimensions = dms)
  
  d <- GetData(key = k,
               normalized = T, 
               pivoting = c(Pivoting(code = rtvar),
                            Pivoting(code = ptvar),
                            Pivoting(code = itmvar),
                            Pivoting(code = elevar),
                            Pivoting(code = yrvar)
               ))
  
  d
}
