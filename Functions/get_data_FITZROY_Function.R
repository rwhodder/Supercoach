
################# GET DATA AND CLEAN FUNCTIONS #################
 
# This script reads data from fitzRoy and manupulates in into a clean dataset from 2002 onwards

############## 
#LOAD PACKAGES
##############

library(tidyverse)
library(dplyr)
library(elo)
library(lubridate)
library(fitzRoy)
library(RMySQL)
library(lubridate)




#######################
# GET DATA FROM FITZROY
#######################



GetDataCleanFitzRoy <- function() {

  # get current year for data retriveal end point
  current_date <- Sys.Date()
  current_year <- year(current_date)
  
  # get player stats from fitzRoy
  dat <- fitzRoy::get_fryzigg_stats(start = 2002, end = current_year)
  
  # connect to mySQL db and get player info
  mydb <- dbConnect(MySQL(), user = "root", password = "yourpassword", dbname = "dfs_db", host = "localhost")
  player_info           <- dbReadTable(mydb, "player_id")
  

 # Clean data
  
  player_stats <- 
    dat %>%
    
  # formats Round
    mutate(round  = case_when(match_round == "Grand Final" ~ 28,                              
                              match_round == "Preliminary Final" ~ 27,
                              match_round == "Preliminary Finals" ~ 27,
                              match_round == "Semi Final" ~ 26,
                              match_round == "Semi Finals" ~ 26,
                              match_round == "Elimination Final" ~ 25,
                              match_round == "Qualifying Final" ~ 25,
                              match_round == "Finals Week 1" ~ 25,
                              match_round == "24" ~ 24,
                              match_round == "23" ~ 23,
                              match_round == "22" ~ 22,
                              match_round == "21" ~ 21,
                              match_round == "20" ~ 20,
                              match_round == "19" ~ 19,
                              match_round == "18" ~ 18,
                              match_round == "17" ~ 17,
                              match_round == "16" ~ 16,
                              match_round == "15" ~ 15,
                              match_round == "14" ~ 14,
                              match_round == "13" ~ 13,
                              match_round == "12" ~ 12,
                              match_round == "11" ~ 11,
                              match_round == "10" ~ 20,
                              match_round == "9" ~ 9,
                              match_round == "8" ~ 8,
                              match_round == "7" ~ 7,
                              match_round == "6" ~ 6,
                              match_round == "5" ~ 5,
                              match_round == "4" ~ 4,
                              match_round == "3" ~ 3,
                              match_round == "2" ~ 2,
                              match_round == "1" ~ 1),
           
      # format player name     
            player = paste0(player_first_name, " ", player_last_name),
            player = case_when(player == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
                                     player == "Daniel Butler" ~ "Dan Butler",
                                     player == "Edward Curnow" ~ "Ed Curnow",
                                     player == "Sam P-Seton" ~ "Sam Petrevski-Seton",
                                     player == "Lachlan Plowman" ~ "Lachie Plowman",
                                     player == "Anthony M-Tipungwuti" ~ "Anthony McDonald-Tipungwuti",
                                     player == "Mitchell Brown" ~ "Mitch Brown",
                                     player == "Matthew Dea" ~ "Matt Dea",
                                     player == "Joshua Green" ~ "Josh Green",
                                     player == "Joshua Begley" ~ "Josh Begley",
                                     player == "Zachary Merrett" ~ "Zach Merrett",
                                     player == "David MacKay" ~ "David Mackay",
                                     player == "Cameron E-Yolmen" ~ "Cam Ellis-Yolmen",
                                     player == "Patrick McCartin" ~ "Paddy McCartin",
                                     player == "Nicholas Robertson" ~ "Nick Robertson",
                                     player == "Joshua Walker" ~ "Josh Walker",
                                     player == "Cameron Rayner" ~ "Cam Rayner",
                                     player == "Oliver Wines" ~ "Ollie Wines",
                                     player == "Darcy B-Jones" ~ "Darcy Byrne-Jones",
                                     player == "Sam P-Pepper" ~ "Sam Powell-Pepper",
                                     player == "Thomas Jonas" ~ "Tom Jonas",
                                     player == "Nathan Fyfe" ~ "Nat Fyfe",
                                     player == "Cameron McCarthy" ~ "Cam McCarthy",
                                     player == "Matthew Rosa" ~ "Matt Rosa",
                                     player == "Lachlan Weller" ~ "Lachie Weller",
                                     player == "Darcy Macpherson" ~ "Darcy MacPherson",
                                     player == "Nicholas Holman" ~ "Nick Holman",
                                     player == "Jamie MacMillan" ~ "Jamie Macmillan",
                                     player == "Ed V-Willis" ~ "Ed Vickers-Willis",
                                     player == "Luke D-Uniacke" ~ "Luke Davies-Uniacke",
                                     player == "Jaeger O'Meara" ~ "Jaeger OMeara",
                                     player == "Tim O'Brien" ~ "Tim OBrien",
                                     player == "Benjamin Stratton" ~ "Ben Stratton",
                                     player == "Samuel Murray" ~ "Sam Murray",
                                     player == "Will H-Elliott" ~ "Will Hoskin-Elliott",
                                     player == "Joshua Kelly" ~ "Josh Kelly",
                                     player == "Matthew De Boer" ~ "Matthew de Boer",
                                     player == "Samuel Reid" ~ "Sam Reid",
                                     player == "Lachlan Hunter" ~ "Lachie Hunter",
                                     player == "Jackson Macrae" ~ "Jack Macrae",
                                     player == "Matthew Suckling" ~ "Matt Suckling",
                                     player == "Timothy English" ~ "Tim English",
                                     player == "Thomas Liberatore" ~ "Tom Liberatore",
                                     player == "Joshua Wagner" ~ "Josh Wagner",
                                     player == "Alex N-Bullen" ~ "Alex Neal-Bullen",
                                     player == "Mitchell Hannan" ~ "Mitch Hannan",
                                     player == "Gary Jnr Ablett" ~ "Gary Ablett",
                                     player == "Mitchell Duncan" ~ "Mitch Duncan",
                                     player == "Lachlan Fogarty" ~ "Lachie Fogarty",
                                     player == "Thomas Stewart" ~ "Tom Stewart",
                                     player == "Dominic Sheed" ~ "Dom Sheed",
                                     player == "Mark Lecras" ~ "Mark LeCras",
                                     player == "Bradley Sheppard" ~ "Brad Sheppard",
                                     player == "Nicholas Naitanui" ~ "Nic Naitanui",
                                     player == "Josh P. Kennedy" ~ "Josh Kennedy",
                                     player == "Cameron O'Shea" ~ "Cameron OShea",
                                     player == "Matthew Scharenberg" ~ "Matt Scharenberg",
                                     player == "Timothy Broomhead" ~ "Tim Broomhead",
                                     player == "Mitchell Crowden" ~ "Mitch Crowden",
                                     player == "Daniel Hannebery" ~ "Dan Hannebery",
                                     player == "Mark O'Connor" ~ "Mark OConnor",
                                     player == "Thomas Sheridan" ~ "Tom Sheridan",
                                     player == "Nicholas Coffield" ~ "Nick Coffield",
                                     player == "William Langford" ~ "Will Langford",
                                     player == "Mitchell Wallis" ~ "Mitch Wallis",
                                     player == "Jordan De Goey" ~ "Jordan de Goey",
                                     player == "Nicholas Graham" ~ "Nick Graham",
                                     player == "Lochie O'Brien" ~ "Lochie OBrien",
                                     player == "Joshua Kennedy" ~ "Josh Kennedy",
                                     player == "George H-Smith" ~ "George Horlin-Smith",
                                     player == "Thomas Boyd" ~ "Tom Boyd",
                                     player == "Daniel Robinson" ~ "Dan Robinson",
                                     player == "Nicholas Shipley" ~ "Nick Shipley",
                                     player == "Christopher Mayne" ~ "Chris Mayne",
                                     player == "Samuel Wright" ~ "Sam Wright",
                                     player == "Edward Phillips" ~ "Ed Phillips",
                                     player == "Mitchell Lewis" ~ "Mitch Lewis",
                                     player == "Matthew Buntine" ~ "Matt Buntine",
                                     player == "Thomas Murphy" ~ "Tom Murphy",
                                     player == "Joshua Schoenfeld" ~ "Josh Schoenfeld",
                                     player == "Bradley Lynch" ~ "Brad Lynch",
                                     player == "Harrison Petty" ~ "Harry Petty",
                                     player == "Jonathan O'Rourke" ~ "Jonathan ORourke",
                                     player == "Jay K-Harris" ~ "Jay Kennedy-Harris",
                                     player == "Colin O'Riordan" ~ "Colin ORiordan",
                                     player == "Lachlan Henderson" ~ "Lachie Henderson",
                                     player == "Zachary Williams" ~ "Zac Williams",
                                     player == "Samuel Collins" ~ "Sam Collins",
                                     player == "Lachlan Schultz" ~ "Lachie Schultz",
                                     player == "Reilly O'Brien" ~ "Reilly OBrien",
                                     player == "Zachary Clarke" ~ "Zac Clarke",
                                     player == "Justin Mcinerney" ~ "Justin McInerney",
                                     player == "Robert Young" ~ "Robbie Young",
                                     player == "Mitchell Hinge" ~ "Mitch Hinge",
                                     player == "Callum C-Jones" ~ "Callum Coleman-Jones",
                                     player == "Bradley Scheer" ~ "Brad Scheer",
                                     player == "Derek E-Smith" ~ "Derek Eggmolesse-Smith",
                                     player == "Ian Hill" ~ "Bobby Hill",
                                     player == "Joshua Deluca" ~ "Josh Deluca",
                                     player == "Brandon Z-Thatcher" ~ "Brandon Zerk-Thatcher",
                                     player == "Zachary Sproule" ~ "Zach Sproule",
                                     player == "Thomas Berry" ~ "Tom Berry",
                                     player == "Lachlan Ash" ~ "Lachie Ash",
                                     player == "Harrison Jones" ~ "Harry Jones",
                                     player == "Minairo Frederick" ~ "Michael Frederick",
                                     player == "Matthew Ling" ~ "Matt Ling",
                                     player == "Bradley Close" ~ "Brad Close",
                                     player == "Xavier O'Halloran" ~ "Xavier OHalloran",
                                     player == "Matt Cottrell" ~ "Matthew Cottrell",
                                     player == "Thomas Hutchesson" ~ "Tom Hutchesson",
                                     player == "Xavier O'Neill" ~ "Xavier ONeill",
                                     player == "Matt Owies" ~ "Matthew Owies",
                                     TRUE ~ player),
      # format team name
      player_team = case_when(player_team == "Geelong"                ~ "GEE",
                              player_team == "Hawthorn"               ~ "HAW",
                              player_team == "Brisbane Lions"         ~ "BRI",
                              player_team == "Collingwood"            ~ "COL",
                              player_team == "North Melbourne"        ~ "NME",
                              player_team == "Fremantle"              ~ "FRE",
                              player_team == "Essendon"               ~ "ESS",
                              player_team == "Carlton"                ~ "CAR",
                              player_team == "Sydney"                 ~ "SYD",
                              player_team == "Gold Coast"             ~ "GCS",
                              player_team == "Greater Western Sydney" ~ "GWS",
                              player_team == "Melbourne"              ~ "MEL",
                              player_team == "Western Bulldogs"       ~ "WBD",
                              player_team == "West Coast"             ~ "WCE",
                              player_team == "St Kilda"               ~ "STK",
                              player_team == "Adelaide"               ~ "ADE",
                              player_team == "Port Adelaide"          ~ "POR",
                              player_team == "Richmond"               ~ "RIC") ,
      # create season and helper variables 
         season = year(match_date),
         helper = paste0(player,"_", season,"_", round,"_",player_team)) %>%
      
      # format variable names           
         select(helper, 
           id = player_id,
           player,
           first_name = player_first_name,
           surname = player_last_name,
           specific_position = player_position,
           height = player_height_cm,
           weight = player_weight_kg,
           jumper_number = guernsey_number,
           season,
           round,
           team = player_team,
           match_id,
           date = match_date,
           start_time = match_local_time,
           venue = venue_name,
           attendance = match_home_team,
           away_team = match_away_team,
           home_score = match_home_team_score,
           away_score = match_away_team_score,
           weather_type = match_weather_temp_c,
           temp = match_weather_temp_c, 
           dfs_score = afl_fantasy_score,
           supercoach = supercoach_score,
           rating_points,
           kicks,
           handballs,
           marks,
           tackles,
           hitouts,
           goals,
           behinds,
           FF = free_kicks_for,
           FA = free_kicks_against,
           disposals,
           effective_disposals,
           effective_kicks,
           disposal_efficiency = disposal_efficiency_percentage,
           CP = contested_possessions,
           UP = uncontested_possessions, 
           f50_ground_ball_gets,
           ground_ball_gets,
           clangers,
           turnovers,
           intercepts,
           ruck_contests,
           hitouts_to_advantage,
           hitout_win_percentage,
           contested_marks,
           marks_I50 = marks_inside_fifty,
           intercept_marks,
           marks_on_lead,
           clearances,
           centre_clearances,
           stoppage_clearances,
           R50 = rebounds,
           I50 = inside_fifties,
           score_involvements,
           score_launches,
           shots_at_goal,
           goal_assists,
           metres_gained,
           one_percenters, 
           bounces,
           spoils,
           TOG = time_on_ground_percentage,
           pressure_acts,
           tackles_inside_fifty,
           contest_def_losses,
           contest_def_one_on_ones,
           contest_off_one_on_ones,
           contest_off_wins,
           def_half_pressure_acts,
           brownlow_votes, 
           everything())
  
  
  # Filter out player name and surname form player_info dataframe, only need position and id
  player_info <- player_info %>% select(id = player_id, position)
  
  # merge player_info into player_stats_merged
  player_stats_merged <- merge(player_info, player_stats, by = "id")
                                                     

  
  
  }
