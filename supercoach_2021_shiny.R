
library(shiny)
library(tidyverse)
library(DT)


# Load data
data <- read_csv("sc_data_for_shiny.csv")


ui <- fluidPage(                                                     # allows for different size screens

  # App Title ---  
  titlePanel("Supercoach Optimiser"),
  
  # Sidebar layout with input and definitions
  sidebarLayout(                                                    # this provides a side bar and a main panel
  
      # Sidebar panel for inputs  
      sidebarPanel(                                                   # specifies what goes in the sidebar
      
          # Input: sliders for adjustment
          shiny::textInput(inputId = "proj_score", label = "proj_score"),
          shiny::textInput(inputId = "manual_weight", label = "manual_weight"),
          
          shiny::actionButton(inputId = "add", label = "Add"),
          
          shiny::selectInput(inputId = "remove_row", label = "Remove Row",
                             choices = 1:nrow(df)),
          
          shiny::actionButton(inputId = "remove", label = "Remove")

      ),
      
      # Main Panel for displaying outputs ---
    mainPanel(                                                      # specifies what is in the main panel
      
          # Output: table 
          DT::DTOutput(outputId = "table")
    )
    )


)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  mod_df <- shiny::reactiveValues(x = data)
  
  output$table <- DT::renderDT({
    
    isolate(mod_df$x)                                                # We use isolate in the code above because we do not want to have a dependency between the data and the DataTable render function
    
  })
  
  
  shiny::observeEvent(input$add, {
    
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(proj_score    = input$proj_score,
                      manual_weight = input$manual_weight)
      )
    
  })

  proxy <- DT::dataTableProxy('table')
  shiny::observe({
    
    DT::replaceData(proxy, mod_df$x)
    
  })

}
  
  shinyApp(ui, server)
# UI     = front end (what you input)
# SERVER = back end and processing (what you output) 
# imputs are things that people can toggle
# 
# 
# outputs are the people see
# - tables
# - graphs