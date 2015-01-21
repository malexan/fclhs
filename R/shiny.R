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
        sidebarPanel(
                     sliderInput("year", "Year of report", 2007, 2011, 2011,
                                 sep = ""),
                     selectInput("reporter", "Reporter", 
                                 c("Mexico" = 138)),
                     selectInput("pt", "Trade partner", 
                                 c("USA" = 231)),
                     selectInput("vrbl", "Quantity or value",
                                 c("Quantity (tonnes)" = "qdiff",
                                   "Value ($1000)" = "vdiff")),
                     sliderInput("rws", "Groups to display",
                                 1, 20, 10),
                     radioButtons("ctsource", "Source of ComTrade data:",
                                  c("CT public API" = "api",
                                    "FAO SWS" = "sws")),
                     sliderInput("w", "Bin width", .1, 1, .9)),
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
                          top_n(n = input$rws, wt = qdiff) # , input$vrbl)
                        })
      
      output$data <- renderDataTable(data())
      
      reactive({
        data() %>%
          mutate(group = factor(group)) %>%
          group_by(flow) %>% # Workaroud with bug in layer_bars()
          ggvis(~group, ~qdiff / 1000) %>%
          layer_bars(fill = ~flow, stack = T, width = input$w) %>% # No option in ggvis 
          # to plot dodged bars
          add_axis("x", title = "Commodities group IDs") %>%
          add_axis("y", title = "Difference between FAO and UN Comtrade, 1000",
                   title_offset = 50)
      }) %>%
        bind_shiny("bar1")
      
    }
  )
  
}