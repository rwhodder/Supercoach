
library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(lpSolve)
library(gdata)
library(Hmisc)
library(shinydashboard)

# Load and creat data
# dataset <- read_excel("Data/ryan_only.xlsx") %>% select(c(Name, Team, Position, Proj_Score, Price))




ui <- dashboardPage(

  
  
  # App Title ---  
    dashboardHeader(title = "Supercoach Optimiser"),
    
  
    # Sidebar layout with input and definitions
    dashboardSidebar(
      
      fileInput(inputId = "file1", 
                label   = "Choose CSV File"),
      
      
      br(),
      selectInput(inputId = "bench1", label = "Defence Bench 1",  choices = ""),
      selectInput(inputId = "bench2", label = "Defence Bench 2",  choices = ""),
      selectInput(inputId = "bench3", label = "Midfield Bench 1", choices = ""),
      selectInput(inputId = "bench4", label = "Midfield Bench 2", choices = ""),
      selectInput(inputId = "bench5", label = "Midfield Bench 3", choices = ""),
      selectInput(inputId = "bench6", label = "Ruck Bench 1",     choices = ""),
      selectInput(inputId = "bench7", label = "Forward Bench 1",  choices = ""),
      selectInput(inputId = "bench8", label = "Forward Bench 2",  choices = ""),

      br(),
      br(),
      br(),
      # Optimize Button
      actionButton("optimise", "OPTIMISE LINEUP"),
      br(),
      p("PROJECTED SCORE:"),
      textOutput("score")
      ),

      
      # Main Panel for displaying outputs ---
      dashboardBody(                                                      # specifies what is in the main panel
      
      
      
      tableOutput("d1")
      
      
      )
    )


    


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  #############
  # FILE INPUT
  #############
  
  dataset <-  observeEvent(input$file1, {
    
    dataset <- read_csv(input$file1$datapath)
    
    def <- dplyr::filter(dataset, Position == "DEF") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    mid <- dplyr::filter(dataset, Position == "MID") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    ruc <- dplyr::filter(dataset, Position == "RUC") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    fwd <- dplyr::filter(dataset, Position == "FWD") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    
    updateSelectInput(session, "bench1", label = "Defence Bench 1",  choices = def)
    updateSelectInput(session, "bench2", label = "Defence Bench 2",  choices = def)
    updateSelectInput(session, "bench3", label = "Midfield Bench 1", choices = mid)
    updateSelectInput(session, "bench4", label = "Midfield Bench 2", choices = mid)
    updateSelectInput(session, "bench5", label = "Midfield Bench 3", choices = mid)
    updateSelectInput(session, "bench6", label = "Ruck Bench 1",     choices = ruc)
    updateSelectInput(session, "bench7", label = "Forward Bench 1",  choices = fwd)
    updateSelectInput(session, "bench8", label = "Forward Bench 2",  choices = fwd)
    
    dataset
  
  })
  
  
  
  observeEvent(input$optimise, {
  
    dataset <- read_csv(input$file1$datapath)
    
  # creates list of bench players from selectInputs
  bench_players <- c(input$bench1,
                     input$bench2, 
                     input$bench3, 
                     input$bench4, 
                     input$bench5, 
                     input$bench6, 
                     input$bench7, 
                     input$bench8)
 
  # makes a dataframe with bench players ONLY
  bench_players_only    <- dataset %>% dplyr::filter(Name %in% bench_players) %>% select(-Position) %>% unique()       
 
  
  # makes a dataframe with bench players REMOVED 
  bench_players_removed <- dataset %>% dplyr::filter(Name %nin% bench_players)     
  
  
  #############
  # OPTIMISER
  #############
  
  
  # count the unique players
  unique_players = unique(dataset$Name)
  
  # calcualte bench cost
  bench_cost <- sum(bench_players_only$Price)
  
  # define the objective for the solver
  obj = dataset$Proj_Score
  
  # create a constraint matrix for the solver
  con = rbind(t(model.matrix(~ Position + 0, dataset)), #Positions
              t(model.matrix(~ Name     + 0, dataset)), #DupPlayers
              rep(1,nrow(dataset)),                     #TotPlayers
              dataset$Price)                            #MaxSalary
  
  # set the direction for each of the constraints
  dir = c("==", #FWD
          "==", #RUC
          "==", #MID
          "==", #DEF
          rep('<=',length(unique_players)), #DupPlayers
          "==", #TotPlayers
          "<=") #MaxSalary
  
  
  
  # set the limits for the right-hand side of the constraints
  rhs = c(6, #FWD
          6, #RUC
          8, #MID
          2, #DEF
          rep(1,length(unique_players)), #DupPlayers
          22, #TotPlayers
          10000000 - bench_cost)        #MaxSalary
  
  
  # find the optimal solution using the solver
  result = lp("max", obj, con, dir, rhs, all.bin = TRUE)
  
  # create a table for the players that are in optimal solution
  solindex      <- which(result$solution==1)
  optsolution   <- bench_players_removed[solindex,]
  final_dataframe   <- optsolution %>%
                     mutate(Help = case_when(Position == "FWD" ~ 1,
                                             Position == "RUC" ~ 2,
                                             Position == "MID" ~ 3,
                                             Position == "DEF" ~ 4)) %>%
                      arrange(desc(Help)) 
  
  
 sum_price <- (10000000 - sum(optsolution$Price))
 sum_score <- sum(optsolution$Proj_Score)
  
  
  
  ######## THIS RENDERS THE DATAFRAME##################################
 
  output$d1 <- renderTable({ final_dataframe})
  
  output$score  <- renderText({ sum_score })
   
  output$salary <- renderText({ bench_cost })
    
      
    })
    
    
}


shinyApp(ui, server)


  
######### THIS CODE GETS ME 

# observeEvent(input$optimise, {
#   
#   # Run optimiser on dataframe with bench players removed
#   
#   bench_players <- c(input$bench1, input$bench2, input$bench3, input$bench4, input$bench5, input$bench6, input$bench7, input$bench8)
#   
#   
#   reactiveDf <- dataset %>% dplyr::filter(Name %in% bench_players)       # makes a dataframe with bench players removed 
#   
#   
#   output$d1 <- DT::renderDataTable({ DT::datatable(reactiveDf) 
#   })
#   
#   
#   
# })
# 
#   
