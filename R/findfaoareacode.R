#' Looks for FCL commodity codes for given character pattern of commodity description.
#' 
#' @param pattern regular expression. String of length 1.
#' @param ignore.case logical. Ignore case while searching. TRUE by default.
#' @param perl logical. Should pattern use the Perl regular expression engine.
#' 
#' @return data.table with code and description columns
#' @export
#' @import dplyr

findfaoareacode <- function(pattern, 
                            ignore.case = T,
                            perl = T) {
  
  if(length(pattern) != 1) stop("Argument pattern must be length of one")
  
  if(ignore.case) pattern <- stringr::ignore.case(pattern)
  if(perl) pattern <- stringr::perl(pattern)
  
  d <- fclhs::faoareanames %>%
    filter(stringr::str_detect(name, pattern)) %>% #NSE here?
    select_(faoareacode = ~fao, faoareaname = ~name)
  
  if(nrow(d) > 1) warning(paste0("Multiple matches: ", 
                                 paste(d$faoareaname, collapse = ", ")))
  
  d$faoareacode
}