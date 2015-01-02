#' Function to run an interactive Shiny-app to explore differences between 
#' FCL and HS datasets.
#' 
#' @import shiny
#' @import dplyr
#' @import ggvis
#' @export

barchart <- function() {
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("w", "Bin width", 1, 20, 10),
                     sliderInput("year", "Year", 2007, 2011, 2011,
                                 format = "####"),
                     selectInput("reporter", "Reporter", 
                                 c("Mexico" = 138)),
                     selectInput("pt", "Partner", 
                                 c("USA" = 231)),
                     selectInput("vrbl", "Quantity/Value",
                                 c("Quantity (tonnes)" = "qdiff",
                                   "Value ($1000)" = "vdiff")),
                     sliderInput("rws", "Groups to display",
                                 1, 100, 50)),
        mainPanel(ggvisOutput("hist1"),
                  dataTableOutput("data"))
      )
    ), 
    server = function(input, output) {
      data <- reactive(read.table(system.file("extdata", 
                                            "mxc_2011_2007_all_all.csv.gz", 
                                            package = "fclhs"), 
                                header = T,
                                sep = ',', stringsAsFactors = F,
                                nrows = 63582 # drop final line with timestamp
      ) %>%
        convertfao3(area = input$reporter, 
                    partner = input$pt, 
                    year = input$year) %>%
        joincompared(getunct(area = unctarea(input$reporter),
                             year = input$year,
                             partner = unctarea(input$pt), 
                             compact = F) %>%
                       convertunctapi()) %>% 
        arrange_(paste0("desc(", input$vrbl, ")")) %>%
        top_n(input$rws, qdiff)
      )
      
      output$data <- renderDataTable(data())

      reactive({faithful %>% ggvis(~waiting)  %>%
        layer_histograms(width = input$w) }) %>%
        bind_shiny("hist1")
    }
  )
  
}