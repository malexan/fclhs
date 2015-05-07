#' Description of FCL-item
#' 
#' @param item Numeric or character vector of FCL-codes. Character vector will 
#' be converted into numeric.
#' @return Character vector with descriptions
#' 
#' @export
descfcl <- function(item) {
  if(is.character(item)) item <- as.numeric(item)
  if(length(item) > 1) return(unlist(lapply(item, descfcl)))
  desc <- fclhs::fcl[fclhs::fcl$fcl == item, "fcltitle"]
  if(length(desc) == 0) desc <- NA
  desc
}