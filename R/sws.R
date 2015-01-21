### Functions to work with SWS

#' Get Comptrade trading data from SWS.
#' 
#' @import dplyr
#' @import faosws
#' @export
getswsct <- function(area, year, partner, item #, 
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
  
  if(!missing(partner) & !is.character(partner)) partner <- as.character(partner)
  if(!missing(area) & !is.character(area)) area <- as.character(area)
  if(!missing(year) & !is.character(year)) year <- as.character(year)
  if(!missing(item) & !is.character(item)) item <- as.character(item)
  
  if(missing(partner)) {
    partner <- GetCodeList(dmn, dtset, ptvar)$code
  }
  
  if(missing(item)) {
    item <- GetCodeList(dmn, dtset, itmvar)$code
    
    # Select HS code of length 6
    item <- item[!is.na(item) & nchar(item) == 6]
  }
  
  dms <- list(Dimension(name = rtvar,
                        keys = area),
              Dimension(name = ptvar,
                        keys = partner),
              Dimension(name = itmvar,
                        keys = item),
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


#' Function ping send one ping-request to a server and returns TRUE 
#' if gets a response.
#' 
#' @source \url{ttps://github.com/mkao006/r_style_fao/blob/master/functions/sws_query.r}

ping <- function(x, stderr = FALSE, stdout = FALSE, ...){
  
  # Windows and unix pings have different argnames for count
  pingcountarg <- ifelse(.Platform$OS.type == "unix", "c", "n")
  
  pingvec <- system2("ping",
                     paste0('-', pingcountarg, ' 1 ', x),
                     stderr,
                     stdout, ...)
  
  pingvec == 0
}

#' Run jnc script to establish VPN-connection with FAO network.
#' 
#' Juniper VPN software has to be installed.
#' 
startjnc <- function(path = "/usr/local/bin/", profile = "fao") {
  if(Platform$OS.type != "unix") stop("Please set up a VPN connection with FAO intranet manually")
  system(paste0(path, "jnc --nox ", profile))
}