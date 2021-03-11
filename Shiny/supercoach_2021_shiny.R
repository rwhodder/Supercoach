
library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(lpSolve)
library(gdata)
library(Hmisc)
library(shinydashboard)
library(gfonts)



# Load and creat data
# dataset <- read_excel("Data/ryan_only.xlsx") %>% select(c(Name, Team, Position, Proj_Score, Price))



ui <-  
  dashboardPage(

    
  
  # App Title ---  
    dashboardHeader(title = "Supercoach Optimizer"),
    
  
    # Sidebar layout with input and definitions
    dashboardSidebar( 
      
    
     
      fileInput(inputId = "file1", 
                label   = "Choose CSV File"),
      
      
      br(),
      selectInput(inputId = "bench1", label = "Defence Bench 1",  choices = "", width="200px"),
     
      selectInput(inputId = "bench2", label = "Defence Bench 2",  choices = "", width="200px"),
      
      selectInput(inputId = "bench3", label = "Midfield Bench 1", choices = "", width="200px"),
      selectInput(inputId = "bench4", label = "Midfield Bench 2", choices = "", width="200px"),
      selectInput(inputId = "bench5", label = "Midfield Bench 3", choices = "", width="200px"),
      
      selectInput(inputId = "bench6", label = "Ruck Bench 1",     choices = "", width="200px"),
      
      selectInput(inputId = "bench7", label = "Forward Bench 1",  choices = "", width="200px"),
      selectInput(inputId = "bench8", label = "Forward Bench 2",  choices = "", width="200px"),
      
      tags$head(tags$style(HTML(".selectize-input {height: 25px; width: 200px; font-size: 12px;}"))), # specifies: height, width and text size
      
      br(),
      br(),
      
      # Optimize Button
      actionButton("optimise", "OPTIMIZE LINEUP", style = 'margin:auto', icon("database"),
      # formatting of Action Button
      style = "color: white;                                                                  
                     background-color: #FFC600; 
                     position: relative; 
                     left: 10%;
                     height: 50px;
                     width: 160px;
                     text-align:center;
                     text-indent: -2px;
                     border-radius: 6px;
                     border-width: 2px") ,
      br(),
      # Render Projected Score
      helpText(HTML('<p style="color:white; font-size: 12pt; text-align:center">PROJECTED SCORE</p>')),
      
      textOutput("score"),
      tags$style(type="text/css", "#score { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
      
      # Render Remaining Salary
      helpText(HTML('<p style="color:white; font-size: 12pt; text-align:center">REMAINING SALARY</p>')),
      
      textOutput("salary"),
      tags$style(type="text/css", "#salary { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}")
      
     
      
                  ),

  
      # Main Panel for displaying outputs ---
      dashboardBody(                                                      # specifies what is in the main panel
        fluidRow(
          br(),
          
        column(width = 6,
               box(title = HTML('<p style="color:black; font-size: 12pt; text-align:center">DEFENCE</p>'), width = NULL, tableOutput("d1")),
                br(),
               box(title = HTML('<p style="color:black; font-size: 12pt; text-align:center">MIDFIELD</p>'), width = NULL, tableOutput("m1")),
                br(),
               box(title = HTML('<p style="color:black; font-size: 12pt; text-align:center">RUCK</p>'), width = NULL, tableOutput("r1")),
                br(),
               box(title = HTML('<p style="color:black; font-size: 12pt; text-align:center">FORWARD</p>'), width = NULL, tableOutput("f1"))),
        column(width = 6,
                box(title = HTML('<p style="color:black; font-size: 12pt; text-align:center">POSITIONAL ALLOCATION</p>'), width = NULL, tableOutput("cost")))
      
        )
        
      ),
    tags$head(tags$style(HTML('* {font-family: "lato"};')))
)



    


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  #############
  # FILE INPUT
  #############
  
  observeEvent(input$file1, {
    
    dataset <- read_csv(input$file1$datapath)
    
    def <- dplyr::filter(dataset, Position == "DEF") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    mid <- dplyr::filter(dataset, Position == "MID") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    ruc <- dplyr::filter(dataset, Position == "RUC") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    fwd <- dplyr::filter(dataset, Position == "FWD") %>% dplyr::filter(Price < 207400) %>% select(Name) 
    
    updateSelectInput(session, "bench1", label = "Defence Bench 1",  choices = def, selected = "Jacob Koschitzke")
    updateSelectInput(session, "bench2", label = "Defence Bench 2",  choices = def, selected = "Thomas Highmore")
    updateSelectInput(session, "bench3", label = "Midfield Bench 1", choices = mid, selected = "Tyler Brockman")
    updateSelectInput(session, "bench4", label = "Midfield Bench 2", choices = mid, selected = "Errol Gulden")
    updateSelectInput(session, "bench5", label = "Midfield Bench 3", choices = mid, selected = "Connor Downie")
    updateSelectInput(session, "bench6", label = "Ruck Bench 1",     choices = ruc, selected = "Lloyd Meek")
    updateSelectInput(session, "bench7", label = "Forward Bench 1",  choices = fwd, selected = "James Rowe")
    updateSelectInput(session, "bench8", label = "Forward Bench 2",  choices = fwd, selected = "Tom Fullarton")
    
    
  
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
    unique_players = unique(bench_players_removed$Name)
    
    # calcualte bench cost
    bench_cost <- sum(bench_players_only$Price)
    
    # define the objective for the solver
    obj = bench_players_removed$Proj_Score
    
    # create a constraint matrix for the solver
    con = rbind(t(model.matrix(~ Position + 0, bench_players_removed)), #Positions
                t(model.matrix(~ Name     + 0, bench_players_removed)), #DupPlayers
                rep(1,nrow(bench_players_removed)),                     #TotPlayers
                bench_players_removed$Price)                            #MaxSalary
    
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
            rep(1,length(unique_players)),  # Duplicate Players
            22,                             # Total Players
            10000000 - bench_cost)          # Max Salary
    
    
    # find the optimal solution using the solver
    result = lp("max", obj, con, dir, rhs, all.bin = TRUE)
    
    # create a table for the players that are in optimal solution
    solindex = which(result$solution==1)
    optsolution = bench_players_removed[solindex,]
    optsolution2 <- optsolution %>% mutate("Projected Score" = Proj_Score)
    final_def <- optsolution2 %>% filter(Position == "DEF") %>% arrange(desc("Projected Score")) %>% select(-Proj_Score) 
    final_mid <- optsolution2 %>% filter(Position == "MID") %>% arrange(desc("Projected Score")) %>% select(-Proj_Score)
    final_ruc <- optsolution2 %>% filter(Position == "RUC") %>% arrange(desc("Projected Score")) %>% select(-Proj_Score)
    final_fwd <- optsolution2 %>% filter(Position == "FWD") %>% arrange(desc("Projected Score")) %>% select(-Proj_Score)
 
    sum_price <- (10000000 - (sum(optsolution$Price) + bench_cost))
    sum_score <- sum(optsolution$Proj_Score)
  
    # CALULATE POSITIONAL SPEND
    position_spend <- optsolution2 %>% 
      group_by(Position) %>%
      summarise(Cost = sum(Price),
                "Salary Allocation %" = (Cost/10000000)*100) %>%
      add_row(Position = "BENCH", Cost = bench_cost, "Salary Allocation %" = (bench_cost/10000000)*100)
  
  
  ######## THIS RENDERS THE DATAFRAME##################################
 
  output$d1 <- renderTable({ final_def}, digits = 0)
  output$m1 <- renderTable({ final_mid}, digits = 0)
  output$r1 <- renderTable({ final_ruc}, digits = 0)
  output$f1 <- renderTable({ final_fwd}, digits = 0)
 
  # renders Projected Score and Remaining Salary
  output$score  <- renderText({ sum_score })
   
  output$salary  <- renderText({ sum_price })
    
  
  
  output$cost  <- renderTable({ position_spend }, digits = 0)
      
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
