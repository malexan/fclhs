#' Detect isolated groups of links between HS and FCL classification from 
#' raw data.
#'  
#' @import dplyr

linksgroups <- function(hs, fcl) {
  
  data <- data.frame(hs, fcl) %>%
    mutate_(hs  = ~paste0("hs", hs), #HS and FCL with similar number become one node
            fcl = ~paste0("fcl", fcl)) 
  
  nodes <- data %>%
    reshape2::melt(measure.vars  = c('hs', 'fcl'),
                   variable.name = "classif",
                   value.name    = "code") %>%
    distinct_() %>%
    select_(~code, ~classif)
  
  grph <- data %>%
    igraph::graph.data.frame(vertices = nodes)
  
  grph.df <- igraph::get.data.frame(grph, what = 'vertices')
  
  grph.df$membership <- factor(igraph::clusters(grph)$membership)
  
  grph.df %>%
    filter_(~classif == "hs") %>%
    mutate_(hs = ~factor(stringr::str_extract(name, "\\d.*$"))) %>%
    select_(~hs, ~membership)
}


