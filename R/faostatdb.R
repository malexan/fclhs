#' Sends given SQL-request to FAOSTAT Oracle DB 
#' and returns resulting data.frame
#' 

faostatsql <- function(sql, host = "lprdbwo1", port = 3310, db = "fstp", 
                     user = "demo", pass = "demo", 
                     classpath = "./ojdbc14.jar") {
  if(exists(".ojdbcclasspath") & missing(classpath)) classpath <- .ojdbcclasspath
  
  drv <- RJDBC::JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
              classPath = classpath)
  
  dbstring <- paste0("jdbc:oracle:thin:@", paste(host, port, db, sep = ":"))
  conn <- DBI::dbConnect(drv, dbstring, user = user, password = pass)
  
  df <- DBI::dbGetQuery(conn, sql)
  
  DBI::dbDisconnect(conn)
  
  names(df) <- tolower(names(df))
  
  df
}

#' Converts list of arguments into SQL WHERE statement
#' 
argstowhere <- function(...) {
  filters <- list(...)
  filters <- filters[!sapply(filters, is.null)]
  filters <- sapply(seq_along(filters), function(x) {
    paste0(names(filters)[x], " IN (", paste(filters[[x]], collapse = ","), ")") })
  filters <- paste(filters, collapse = " AND ")
  filters <- paste0(" WHERE ", filters)
  filters
}

#' Returns data from FAOSTAT.TF_SOURCE_*
#' 
#' @export

gettfsource <- function(reporter = NULL, year = NULL, partner = NULL,
                        flow = NULL, 
                        hs   = NULL) {
  if(is.null(year)) stop("Please specify year")
  
  qselect <- "
select * from (
           select REPORT_AREA as reporter, 
           PARTNER_AREA as partner,
           FLOW, 
           trim(ITEM_ORIG) as hs,
           QUANTITY_ORIG as qty,
           OTHER_QUANTITY as qty2,
           VALUE_ORIG as value
  "
  
  qfrom <- paste0(" from FAOSTAT.TF_SOURCE_", year, ") ")
  
  filters <- argstowhere(reporter = reporter, partner = partner, flow = flow, hs = hs)
  
  qfull <- paste0(qselect, qfrom, filters)
  
  faostatsql(qfull)
  
}

#' Returns data from FAOSTAT.TF_VALID
#' 
#' @export
gettfvalid <- function(reporter = NULL, year = NULL, partner = NULL,
                           flow = NULL, 
                           fcl  = NULL) {
  if(is.null(year)) stop("Please specify year")
  
  qselect <- "select * from (select 
                yr as year, 
                flow, 
                report_area as reporter, 
                partner_area as partner, 
                item as fcl, 
                quantity, 
                value  
                from FAOSTAT.TF_VALID 
                where yr = "
  qselect <- paste0(qselect, year, ") ")
  
  filters <- argstowhere(reporter = reporter, partner = partner, flow = flow, fcl = fcl)
  
  qfull <- paste0(qselect, filters)
  
  faostatsql(qfull)
  
}