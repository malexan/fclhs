### Functions to work with SWS

#' Get Comptrade trading data from SWS.
#' 
#' @import dplyr
#' @import faosws
#' @export
getswsct <- function(area = NULL,
                     year = NULL,
                     partner = NULL,
                     item = NULL, 
                     flow = "all", 
                     swshost = "hqlqasws1.hq.un.fao.org",
                     swsport = "8181",
                     swspath = "/sws",
                     sessionid = "3870d0bb-2a46-4525-ba44-d9d246ecf502"
                     #, code = "AG6" #, compact = T,
                     #desc = F, debug = F
                     ) {
  if(!ping(swshost)) startjnc()
  
  # Detecting existence of SWS TestEnvir in calling env
  # by counting swsContext.* vars
  if(length(ls(pos = 1, pattern = "^swsContext\\.")) < 8)
    GetTestEnvironment(paste0("https://", swshost, ":", swsport, swspath),
                       sessionid)
  
  dmn    <- "trade"
  dtset  <- "ct_raw_tf"
  rtvar  <- "reportingCountryM49"
  ptvar  <- "partnerCountryM49"
  itmvar <- "measuredItemHS"
  elevar <- "measuredElementTrade"
  yrvar  <- "timePointYears"

  # TODO replace this !missing & is.ch. It leads to error if missing.
  
  ascharnotnullarg <- function(arg) {
    if(!is.null(arg) & !is.character(arg)) arg <- as.character(arg)
    arg
  }
  
  partner <- ascharnotnullarg(partner)
  area    <- ascharnotnullarg(area)
  year    <- ascharnotnullarg(year)
  item    <- ascharnotnullarg(item)
  
  if(is.null(partner)) {
    partner <- GetCodeList(dmn, dtset, ptvar)$code
  }
  
  if(is.null(item)) {
    item <- GetCodeList(dmn, dtset, itmvar)$code
    
    # Select HS code of length 6
    item <- item[!is.na(item) & nchar(item) == 6]
  }
  
  if(tolower(flow) == "all") ele <- GetCodeList(dmn, dtset, elevar)$code
  
  dms <- list(Dimension(name = rtvar,
                        keys = area),
              Dimension(name = ptvar,
                        keys = partner),
              Dimension(name = itmvar,
                        keys = item),
              Dimension(name = elevar,
                        keys = ele),
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
  if(.Platform$OS.type != "unix") stop("Please set up a VPN connection with FAO intranet manually")
  system(paste0(path, "jnc --nox ", profile))
}