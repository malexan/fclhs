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
  # Check API access limit (No more than 1 per second).
  # Good to add 100 requests per day.
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
               qt = ~TradeQuantity,
              kg = ~NetWeight,
               flow = ~rgCode,
               flowdesc = ~rgDesc,
               value = ~TradeValue,
               qtdesc = ~qtDesc,
               hsdesc = ~cmdDescE)
  if(desc) return(d)
  select_(d,  lazyeval::interp(~-ends_with(x), x = "desc"))
}

#' Convert data extracted from UN Comtrade API to FAOSTAT format.
#' @import dplyr
#' @export
uncttofao <- function(data, 
                      long = T,
                      faonames = T,
                      quantity = F, 
                      netweight = F) {
  if(quantity | netweight) stop("Quantity and netweight are not implemented.")
  if(!("qtdesc" %in% names(data))) {
    stop(paste("No description of quantity in data.", 
               "Please, retrieve data by getunct() with desc=T argument.",
               sep = "\n"))
  }
  data <- data %>%
    mutate_(group = ~hsgroup(hs)) %>%
    filter_(~(#qtdesc == "Weight in kilograms" & # Suppose food in kg only (also litres)
      !is.na(group))) # Drop all HS which not in conversion table
  
  group_by_(~flow, ~year, ~area, ~pt, ~group, ~qtdesc) %>%
    summarize_(ctvalue = ~sum(value),
               ctkg = ~sum(kg),
               ctqt = ~sum(qt)) %>%
    ungroup() %>%
    mutate_(area = ~faoarea(area),
            pt = ~faoarea(pt),
            ctvalue = ~round(ctvalue / 1000), # to 1000 USD
            ctqt = ~round(ctqt / 1000),  # to tonnes
            ctkg = ~round(ctkg / 1000)) # to tonnes
  data
}

