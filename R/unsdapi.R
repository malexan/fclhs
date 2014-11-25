#' Get trading data via UNSD API.
#' 
#' @import dplyr
#' @importFrom lubridate ymd_hms seconds now
#' @export
getunct <- function(area, year, partner = "all", 
                    flow = "all", code = "AG6", compact = T,
                    desc = F, debug = F) {
  # http://comtrade.un.org/data/doc/api/
  
  # Max number of commodity codes
  maxcc <- 20
  if(length(code) > maxcc) {
    l <- split(code, ceiling(seq_along(code)/maxcc))
    
    # If to run in parallel change from lapply to fornext because of 
    # limits of UNCT API (1 request per second)
    reslist <- lapply(l, function(x, ...) getunsd(code = x, ...),
                      area, year, partner, flow, compact, desc, debug)
    if(debug) return(reslist)
    return(dplyr::rbind_all(reslist))
  }
  
  
  domain <- "http://comtrade.un.org/"
  path <- "api/get"
  query <- list(fmt = 'json',
                r = area, # reporting area
                freq = "A", # Annual
                ps = year,
                px = "H3", # HS2007
                p = partner, # partner area
                rg = flow, # trade regime/flow
                cc = paste(code, sep ="", collapse = ","), # classif.code. 
                #6 digits for HS here
                # paste() if 
                #there are several codes
                max = 50000,
                head = "M" # Machine readable instead of Human readable
  )
  # Check access limit
  if(!exists(".lastUNSDAPIaccess")) .lastUNSDAPIaccess <<- lubridate::now()
  if(exists(".lastUNSDAPIaccess")) {
    if(ymd_hms(.lastUNSDAPIaccess) + seconds(1) > ymd_hms(now())) {
      pause <- difftime(ymd_hms(.lastUNSDAPIaccess) + 
                          seconds(1), ymd_hms(now()), unit = "secs")
      Sys.sleep(as.numeric(pause))
    }
    .lastUNSDAPIaccess <<- now()
  }
  
  r <- httr::GET(domain, path = path, query = query)
  r <- jsonlite::fromJSON(httr::content(r, as = "text"))
  if(debug) return(list(query, r))
  if(r$validation$status$name != "Ok") message(r$validation$message)
  if(r$validation$status$name == "Ok" &
       !is.data.frame(r$dataset)) return(data.frame(year = integer(0)))
  if(r$validation$count$value != nrow(r$dataset)) message(
    paste("Expected size of dataset (", r$validation$count$value, 
          ") is not equal to size of returned dataset (",
          nrow(r$dataset), ").", sep = ""))
  if(!compact) return(r$dataset)
  d <- select_(r$dataset, year = ~yr,
               hs = ~cmdCode, 
               area = ~rtCode,
               pt = ~ptCode,
               qt = ~TradeQuantity, # NetWeight exists also!!!
               flow = ~rgCode,
               flowdesc = ~rgDesc,
               value = ~TradeValue,
               qtdesc = ~qtDesc,
               hsdesc = ~cmdDescE)
  if(desc) return(d)
  select_(d,  lazyeval::interp(~-ends_with(x), x = "desc"))
}



getunsdfao <- function(area, year, partner, 
                       flow = "all", code, ...) {
  area <- unctarea(area)
  partner <- unctarea(partner)
  code <- fcl2hs(code)
  d <- getunct(area = area,
               year = year,
               partner = partner,
               flow = flow,
               code = code,
               ...)
  d
  #   d %>%
  #     mutate(fcl 
}

