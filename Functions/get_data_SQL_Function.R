
#############################
# GET DATA FROM MYSQL DATABSE
#############################

GetDataCleanMySQL <- function() {
  
  
  # Connect to MySQL dfs_db
  mydb <- dbConnect(MySQL(), user = "root", password = "yourpassword", dbname = "dfs_db", host = "localhost")
  
  # Get required tables
  advanced_player_stats <- dbReadTable(mydb, "advanced_player_stats") # advanced_player_stats
  player_stats          <- dbReadTable(mydb, "player_stats")          # player_stats
  
  
  # Combine advanced_player_stats and player_stats
  # create unique helper column to merge columns with: use a space separator between first and surname to be able to
  # connect with advanced_player_stats which has first and surname as one combined column
  
  # CLEAN PLAYER_STATS
  
  
  player_stats <- 
    player_stats %>%
    mutate(Round  = case_when(Round == "GF" ~ 28,                              # formats Round and converts to numeric
                              Round == "PF" ~ 27,
                              Round == "SF" ~ 26,
                              Round == "EF" ~ 25,
                              Round == "QF" ~ 25,
                              Round == "24" ~ 24,
                              Round == "23" ~ 23,
                              Round == "22" ~ 22,
                              Round == "21" ~ 21,
                              Round == "20" ~ 20,
                              Round == "19" ~ 19,
                              Round == "18" ~ 18,
                              Round == "17" ~ 17,
                              Round == "16" ~ 16,
                              Round == "15" ~ 15,
                              Round == "14" ~ 14,
                              Round == "13" ~ 13,
                              Round == "12" ~ 12,
                              Round == "11" ~ 11,
                              Round == "10" ~ 20,
                              Round == "9" ~ 9,
                              Round == "8" ~ 8,
                              Round == "7" ~ 7,
                              Round == "6" ~ 6,
                              Round == "5" ~ 5,
                              Round == "4" ~ 4,
                              Round == "3" ~ 3,
                              Round == "2" ~ 2,
                              Round == "1" ~ 1),
           Team = case_when(Playing.for == "Brisbane Lions"          ~ "BRI",
                            Playing.for == "Fremantle"               ~ "FRE",
                            Playing.for == "Adelaide"                ~ "ADE",
                            Playing.for == "Carlton"                 ~ "CAR",
                            Playing.for == "Essendon"                ~ "ESS",
                            Playing.for == "Geelong"                 ~ "GEE",
                            Playing.for == "Hawthorn"                ~ "HAW",
                            Playing.for == "West Coast"              ~ "WCE",
                            Playing.for == "North Melbourne"         ~ "NME",
                            Playing.for == "Port Adelaide"           ~ "POR",
                            Playing.for == "Gold Coast"              ~ "GCS",
                            Playing.for == "Sydney"                  ~ "SYD",
                            Playing.for == "Collingwood"             ~ "COL",
                            Playing.for == "St Kilda"                ~ "STK",
                            Playing.for == "Western Bulldogs"        ~ "WBD",
                            Playing.for == "Richmond"                ~ "RIC",
                            Playing.for == "Melbourne"               ~ "MEL",
                            Playing.for == "Greater Western Sydney"  ~ "GWS",
                            TRUE ~ Playing.for),
           helper = paste0(First.name," ",Surname,"_",Season,"_",Round,"_",Team)) %>%   # creates helper to bind to player_stats
    select(helper, everything())
  
  
  unique(advanced_player_stats$Team)
  # CLEAN ADVANCED_PLAYER_STATS
  advanced_player_stats <- 
    advanced_player_stats %>% 
    # formats Round and converts to numeric
    mutate(Round  = case_when(Round == "Grand Final" ~ 28,                     
                              Round == "Preliminary Final" ~ 27,
                              Round == "Semi Final" ~ 26,
                              Round == "Elimination Final" ~ 25,
                              Round == "Qualifying Final" ~ 25,
                              Round == "Round 24" ~ 24,
                              Round == "Round 23" ~ 23,
                              Round == "Round 22" ~ 22,
                              Round == "Round 21" ~ 21,
                              Round == "Round 20" ~ 20,
                              Round == "Round 19" ~ 19,
                              Round == "Round 18" ~ 18,
                              Round == "Round 17" ~ 17,
                              Round == "Round 16" ~ 16,
                              Round == "Round 15" ~ 15,
                              Round == "Round 14" ~ 14,
                              Round == "Round 13" ~ 13,
                              Round == "Round 12" ~ 12,
                              Round == "Round 11" ~ 11,
                              Round == "Round 10" ~ 20,
                              Round == "Round 9" ~ 9,
                              Round == "Round 8" ~ 8,
                              Round == "Round 7" ~ 7,
                              Round == "Round 6" ~ 6,
                              Round == "Round 5" ~ 5,
                              Round == "Round 4" ~ 4,
                              Round == "Round 3" ~ 3,
                              Round == "Round 2" ~ 2,
                              Round == "Round 1" ~ 1),
           # formats player name to be same as player_stats
           Player = case_when(Player == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
                              Player == "Daniel Butler" ~ "Dan Butler",
                              Player == "Edward Curnow" ~ "Ed Curnow",
                              Player == "Sam P-Seton" ~ "Sam Petrevski-Seton",
                              Player == "Lachlan Plowman" ~ "Lachie Plowman",
                              Player == "Anthony M-Tipungwuti" ~ "Anthony McDonald-Tipungwuti",
                              Player == "Mitchell Brown" ~ "Mitch Brown",
                              Player == "Matthew Dea" ~ "Matt Dea",
                              Player == "Joshua Green" ~ "Josh Green",
                              Player == "Joshua Begley" ~ "Josh Begley",
                              Player == "Zachary Merrett" ~ "Zach Merrett",
                              Player == "David MacKay" ~ "David Mackay",
                              Player == "Cameron E-Yolmen" ~ "Cam Ellis-Yolmen",
                              Player == "Patrick McCartin" ~ "Paddy McCartin",
                              Player == "Nicholas Robertson" ~ "Nick Robertson",
                              Player == "Joshua Walker" ~ "Josh Walker",
                              Player == "Cameron Rayner" ~ "Cam Rayner",
                              Player == "Oliver Wines" ~ "Ollie Wines",
                              Player == "Darcy B-Jones" ~ "Darcy Byrne-Jones",
                              Player == "Sam P-Pepper" ~ "Sam Powell-Pepper",
                              Player == "Thomas Jonas" ~ "Tom Jonas",
                              Player == "Nathan Fyfe" ~ "Nat Fyfe",
                              Player == "Cameron McCarthy" ~ "Cam McCarthy",
                              Player == "Matthew Rosa" ~ "Matt Rosa",
                              Player == "Lachlan Weller" ~ "Lachie Weller",
                              Player == "Darcy Macpherson" ~ "Darcy MacPherson",
                              Player == "Nicholas Holman" ~ "Nick Holman",
                              Player == "Jamie MacMillan" ~ "Jamie Macmillan",
                              Player == "Ed V-Willis" ~ "Ed Vickers-Willis",
                              Player == "Luke D-Uniacke" ~ "Luke Davies-Uniacke",
                              Player == "Jaeger O'Meara" ~ "Jaeger OMeara",
                              Player == "Tim O'Brien" ~ "Tim OBrien",
                              Player == "Benjamin Stratton" ~ "Ben Stratton",
                              Player == "Samuel Murray" ~ "Sam Murray",
                              Player == "Will H-Elliott" ~ "Will Hoskin-Elliott",
                              Player == "Joshua Kelly" ~ "Josh Kelly",
                              Player == "Matthew De Boer" ~ "Matthew de Boer",
                              Player == "Samuel Reid" ~ "Sam Reid",
                              Player == "Lachlan Hunter" ~ "Lachie Hunter",
                              Player == "Jackson Macrae" ~ "Jack Macrae",
                              Player == "Matthew Suckling" ~ "Matt Suckling",
                              Player == "Timothy English" ~ "Tim English",
                              Player == "Thomas Liberatore" ~ "Tom Liberatore",
                              Player == "Joshua Wagner" ~ "Josh Wagner",
                              Player == "Alex N-Bullen" ~ "Alex Neal-Bullen",
                              Player == "Mitchell Hannan" ~ "Mitch Hannan",
                              Player == "Gary Jnr Ablett" ~ "Gary Ablett",
                              Player == "Mitchell Duncan" ~ "Mitch Duncan",
                              Player == "Lachlan Fogarty" ~ "Lachie Fogarty",
                              Player == "Thomas Stewart" ~ "Tom Stewart",
                              Player == "Dominic Sheed" ~ "Dom Sheed",
                              Player == "Mark Lecras" ~ "Mark LeCras",
                              Player == "Bradley Sheppard" ~ "Brad Sheppard",
                              Player == "Nicholas Naitanui" ~ "Nic Naitanui",
                              Player == "Josh P. Kennedy" ~ "Josh Kennedy",
                              Player == "Cameron O'Shea" ~ "Cameron OShea",
                              Player == "Matthew Scharenberg" ~ "Matt Scharenberg",
                              Player == "Timothy Broomhead" ~ "Tim Broomhead",
                              Player == "Mitchell Crowden" ~ "Mitch Crowden",
                              Player == "Daniel Hannebery" ~ "Dan Hannebery",
                              Player == "Mark O'Connor" ~ "Mark OConnor",
                              Player == "Thomas Sheridan" ~ "Tom Sheridan",
                              Player == "Nicholas Coffield" ~ "Nick Coffield",
                              Player == "William Langford" ~ "Will Langford",
                              Player == "Mitchell Wallis" ~ "Mitch Wallis",
                              Player == "Jordan De Goey" ~ "Jordan de Goey",
                              Player == "Nicholas Graham" ~ "Nick Graham",
                              Player == "Lochie O'Brien" ~ "Lochie OBrien",
                              Player == "Joshua Kennedy" ~ "Josh Kennedy",
                              Player == "George H-Smith" ~ "George Horlin-Smith",
                              Player == "Thomas Boyd" ~ "Tom Boyd",
                              Player == "Daniel Robinson" ~ "Dan Robinson",
                              Player == "Nicholas Shipley" ~ "Nick Shipley",
                              Player == "Christopher Mayne" ~ "Chris Mayne",
                              Player == "Samuel Wright" ~ "Sam Wright",
                              Player == "Edward Phillips" ~ "Ed Phillips",
                              Player == "Mitchell Lewis" ~ "Mitch Lewis",
                              Player == "Matthew Buntine" ~ "Matt Buntine",
                              Player == "Thomas Murphy" ~ "Tom Murphy",
                              Player == "Joshua Schoenfeld" ~ "Josh Schoenfeld",
                              Player == "Bradley Lynch" ~ "Brad Lynch",
                              Player == "Harrison Petty" ~ "Harry Petty",
                              Player == "Jonathan O'Rourke" ~ "Jonathan ORourke",
                              Player == "Jay K-Harris" ~ "Jay Kennedy-Harris",
                              Player == "Colin O'Riordan" ~ "Colin ORiordan",
                              Player == "Lachlan Henderson" ~ "Lachie Henderson",
                              Player == "Zachary Williams" ~ "Zac Williams",
                              Player == "Samuel Collins" ~ "Sam Collins",
                              Player == "Lachlan Schultz" ~ "Lachie Schultz",
                              Player == "Reilly O'Brien" ~ "Reilly OBrien",
                              Player == "Zachary Clarke" ~ "Zac Clarke",
                              Player == "Justin Mcinerney" ~ "Justin McInerney",
                              Player == "Robert Young" ~ "Robbie Young",
                              Player == "Mitchell Hinge" ~ "Mitch Hinge",
                              Player == "Callum C-Jones" ~ "Callum Coleman-Jones",
                              Player == "Bradley Scheer" ~ "Brad Scheer",
                              Player == "Derek E-Smith" ~ "Derek Eggmolesse-Smith",
                              Player == "Ian Hill" ~ "Bobby Hill",
                              Player == "Joshua Deluca" ~ "Josh Deluca",
                              Player == "Brandon Z-Thatcher" ~ "Brandon Zerk-Thatcher",
                              Player == "Zachary Sproule" ~ "Zach Sproule",
                              Player == "Thomas Berry" ~ "Tom Berry",
                              Player == "Lachlan Ash" ~ "Lachie Ash",
                              Player == "Harrison Jones" ~ "Harry Jones",
                              Player == "Minairo Frederick" ~ "Michael Frederick",
                              Player == "Matthew Ling" ~ "Matt Ling",
                              Player == "Bradley Close" ~ "Brad Close",
                              Player == "Xavier O'Halloran" ~ "Xavier OHalloran",
                              Player == "Matt Cottrell" ~ "Matthew Cottrell",
                              Player == "Thomas Hutchesson" ~ "Tom Hutchesson",
                              Player == "Xavier O'Neill" ~ "Xavier ONeill",
                              Player == "Matt Owies" ~ "Matthew Owies",
                              TRUE ~ Player),
           Team = case_when(Team == "GWS"                     ~ "GWS",
                            Team == "Brisbane"                ~ "BRI",
                            Team == "Fremantle"               ~ "FRE",
                            Team == "Adelaide"                ~ "ADE",
                            Team == "Carlton"                 ~ "CAR",
                            Team == "Essendon"                ~ "ESS",
                            Team == "Geelong"                 ~ "GEE",
                            Team == "Hawthorn"                ~ "HAW",
                            Team == "West Coast"              ~ "WCE",
                            Team == "North Melbourne"         ~ "NME",
                            Team == "Port Adelaide"           ~ "POR",
                            Team == "Gold Coast"              ~ "GCS",
                            Team == "Sydney"                  ~ "SYD",
                            Team == "Collingwood"             ~ "COL",
                            Team == "St Kilda"                ~ "STK",
                            Team == "Western Bulldogs"        ~ "WBD",
                            Team == "Richmond"                ~ "RIC",
                            Team == "Melbourne"               ~ "MEL",
                            Team == "Greater Western Sydney"  ~ "GWS",
                            TRUE ~ Team),
           # creates helper to bind to player_stats
           helper = paste0(Player,"_",Season,"_",Round,"_",Team)) %>%      
    select(helper,                                                # select on variables that are not duplicates in player_stats
           Status, 
           ED, 
           DE, 
           GA...15, 
           D, 
           AF, 
           SC, 
           CCL, 
           SCL, 
           SI, 
           MG, 
           ITC, 
           T5)
  
  
  
  # This checks for players that naming conventions are not lining up - use player_stats as primary table
  #anti_join <- anti_join(advanced_player_stats, player_stats, by = "helper")
  #anti_join <- unique(anti_join$Player)
  
  # MERGE PLAYER_STATS AND ADVANCDED_PLAYER_STATS
  
  player_stats_merged <- merge(advanced_player_stats, player_stats, by = "helper") %>%    # merges by helper column
    select(helper,                                                   # reorder and rename variables    
           ID, 
           first_name = First.name,
           surname = Surname,
           jumper_number = Jumper.No.,
           season = Season,
           round = Round,
           team = Team,
           status = Status,
           date = Date,
           start_time = Local.start.time,
           venue = Venue,
           attendance = Attendance,
           home_score = Home.score,
           away_score = Away.score,
           dfs_score = AF,
           supercoach = SC,
           kicks = Kicks,
           handballs = Handballs,
           marks = Marks,
           tackles = Tackles,
           hit_outs = Hit.Outs,
           goals = Goals,
           behinds = Behinds,
           FF = Frees.For,
           FA = Frees.Against,
           disposals = D,
           effective_disposals = ED,
           disposal_efficiency = DE,
           CP = Contested.Possessions,
           UP = Uncontested.Possessions, 
           clangers = Clangers,
           contested_marks = Contested.Marks,
           marks_I50 = Marks.Inside.50,
           tackles_I50 = T5,
           clearances = Clearances,
           centre_clearances = CCL,
           stoppage_clearances = SCL,
           R50 = Rebounds,
           I50 = Inside.50s,
           score_involvements = SI,
           metres_gained = MG,
           intercepts = ITC,
           goal_assists = Goal.Assists,
           one_percenters = One.Percenters, 
           bounces = Bounces,
           TOG = Time.on.Ground..,
           brownlow_votes = Brownlow.Votes, 
           everything())
  
  
  
  # Filter out player name and surname form player_info dataframe, only need position and id
  
  
  # ---------------- REMOVED THIS PLAYER INFO AS DONT NEED MY OWN POSITIONS (USE SUPERCOACH OF DK INSTEAD)
  # player_info <- player_info %>% select(ID = player_id, position)
  
  # merge player_info into player_stats_merged
  # position_merged <- merge(player_stats_merged, player_info, by = "ID")
  # -----------------
  
}

