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
                                 1, 100, 10)),
        mainPanel(ggvisOutput("bar1"),
                  dataTableOutput("data"))
      )
    ), 
    server = function(input, output) {
      fulldata <- reactive({read.table(system.file("extdata", 
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
                       convertunctapi())
      })
      
      data <- reactive({fulldata() %>%
                          mutate_(inpt = input$vrbl) %>%
                          arrange_(paste0("desc(", input$vrbl, ")")) %>%
                          top_n(input$rws) # , input$vrbl)
                        })
      
      output$data <- renderDataTable(data())
      
      data %>%
        mutate(group = factor(group)) %>%
        group_by(flow) %>% # Workaroud with bug in layer_bars()
        ggvis(~group, ~qdiff) %>%
        layer_bars(fill = ~flow, stack = T) %>% # No option in ggvis 
                                                # to plot dodged bars
        add_axis("x", title = "Commodities group IDs") %>%
        add_axis("y", title = "Difference between FAO and UN Comtrade",
                 title_offset = 50) %>%
        bind_shiny("bar1")
  
    }
  )
  
}