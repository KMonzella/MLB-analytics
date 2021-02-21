# ---------------------------- #
# --- Step 0: program setup--- #
# ---------------------------- #


# remove all objects from workspace
  rm(list = ls())

# installations
  #devtools::install_github("keberwein/mlbgameday", force = TRUE)

# load libraries
  library(jsonlite)
  library(tidyverse)
  library(mlbgameday)
  library(baseballr)
  library(DBI)
  library(RPostgreSQL)  

  
  
# ---------------------------------- #
# --- Step 1: Scrape season data --- #
# ---------------------------------- #
  
  
  # source: https://billpetti.github.io/2020-05-26-build-statcast-database-rstats-version-2.0/ 
  annual_statcast_query <- function(season, startd, endd) {
    
    # create date grid based on start and end date of season
    dates <- seq.Date(as.Date(paste0(season, startd)),
                      as.Date(paste0(season, endd)), by = 'week')
    
    date_grid <- tibble(start_date = dates, 
                        end_date = dates + 6)
    
    # wrap scrape_statcast_savant so that it does not err when reaching day on which no games
    safe_savant <- safely(scrape_statcast_savant)
    
    # call safe_savant for every game date in grid
    payload <- map(.x = seq_along(date_grid$start_date), 
                   ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                     
                     payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                            end_date = date_grid$end_date[.x], 
                                            type = 'pitcher')
                     
                     return(payload)
                   })
    payload_df <- map(payload, 'result')

    # On some dates, we will have no data. Drop if nrows == 0
    number_rows <- map_df(.x = seq_along(payload_df), 
                          ~{number_rows <- tibble(week = .x, 
                                                  number_rows = length(payload_df[[.x]]$game_date))}) %>%
                          filter(number_rows > 0) %>% pull(week)
    
    payload_df_reduced <- payload_df[number_rows]
    
    combined <- payload_df_reduced %>% bind_rows()
    
    # output final filtered file
    return(combined)
    
  }
  
  # call the function for 2019 and 2020 data
  #d2019 <- annual_statcast_query(2019, '-03-20', '-09-29')
  d2020 <- annual_statcast_query(2020, '-07-24', '-09-27')


  
  # ----------------------------------------- #
  # --- Step 2: stack data and run checks --- #
  # ----------------------------------------- #
  
  
  # check n game dates
  # payload <-rbind(d2019,d2020) 
  payload <- d2020
  
  ndays <- payload %>% select(game_date)%>% filter(!is.na(game_date))%>%distinct()%>%pull()%>%as.list()
  message(paste("Number of game dates pulled from baseball savant:" , length(ndays)))
  
  
  # n games
  # pull game_pks for every game on every day in season
  id <- vector(mode = "list", length = length(ndays))
  
  for (i in 1:length(ndays)){
    id[i] <- get_game_pks_mlb(date = as.Date(ndays[[i]]))
  }
  
  gids <- do.call(c, id)
  # remove erring case. need to look into error
  gids <- gids[gids != 631471 & gids != 631472]
  
  message(paste("Number of unique game IDS in data:", length(gids)))
  

  
  # --------------------------------------------------------- #
  # --- Step 3: Start scaping data to match real-time call ---#
  # --------------------------------------------------------- #
  

  # initialize dataframe of results
  stats_api_live_empty_df <- data.frame(NULL)

  # bill petti's code starts here
  #gids <- head(gids) # limit just for sake of testing
  
  # --- call API
  f <- function (game_pk){
    
    # call API
    api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
    
    # load data
    payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
    print(head(payload))
    
    print(message(paste("Current game_pk is: ", game_pk)))
    
    # pull features from payload data - live data
    plays         <- payload$liveData$plays$allPlays$playEvents %>% bind_rows()
    at_bats       <- payload$liveData$plays$allPlays
    current       <- payload$liveData$plays$currentPlay
    
    # pull feature from payload data - game metadata
    game_status   <- payload$gameData$status$abstractGameState
    home_team     <- payload$gameData$teams$home$name
    home_level    <- payload$gameData$teams$home$sport
    home_league   <- payload$gameData$teams$home$league
    away_team     <- payload$gameData$teams$away$name
    away_level    <- payload$gameData$teams$away$sport
    away_league   <- payload$gameData$teams$away$league
    
    # need to look into what this is doing
    list_columns  <- lapply(at_bats, function(x) class(x)) %>% 
      dplyr::bind_rows(.id = "variable") %>% tidyr::gather(key, 
                                                           value) %>% dplyr::filter(value == "list") %>% dplyr::pull(key)
    
    at_bats <- at_bats %>% dplyr::select(-c(one_of(list_columns)))
    
    # building pitch by pitch data
    
    # KM added prints
    print(head(at_bats))
    print(head(plays))
    
    pbp <- plays %>% dplyr::left_join(at_bats, by = c(endTime = "playEndTime"))
    
    # Fill missing values - why is this separate from mutates below? Assess run time differences
    pbp <- pbp %>% tidyr::fill(atBatIndex:matchup.splits.menOnBase, 
                               .direction = "up") %>% 
      dplyr::mutate(game_pk = game_pk, 
                    game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) %>% 
      dplyr::select(game_pk, game_date, everything())
    
    # Fill columns of data frame with values pulled in code above
    pbp <- pbp %>% dplyr::mutate(matchup.batter.fullName  = factor(matchup.batter.fullName), 
                                 matchup.pitcher.fullName = factor(matchup.pitcher.fullName), 
                                 atBatIndex               = factor(atBatIndex)) %>% 
      dplyr::mutate(home_team                = home_team, 
                    home_level_id            = home_level$id, 
                    home_level_name          = home_level$name, 
                    #home_parentOrg_id        = payload$gameData$teams$home$parentOrgId, 
                    #home_parentOrg_name      = payload$gameData$teams$home$parentOrgName, 
                    home_league_id           = home_league$id, 
                    home_league_name         = home_league$name, 
                    away_team                = away_team, 
                    away_level_id            = away_level$id, 
                    away_level_name          = away_level$name, 
                    #away_parentOrg_id        = payload$gameData$teams$away$parentOrgId, 
                    #away_parentOrg_name      = payload$gameData$teams$away$parentOrgName, 
                    away_league_id           = away_league$id, 
                    away_league_name         = away_league$name, 
                    batting_team             = factor(ifelse(about.halfInning == "bottom", home_team, away_team)), 
                    fielding_team            = factor(ifelse(about.halfInning == "bottom", away_team, home_team)))
    
    pbp <- pbp %>% dplyr::arrange(desc(atBatIndex), desc(pitchNumber))
    pbp <- pbp %>% dplyr::group_by(atBatIndex) %>% 
      dplyr::mutate(last.pitch.of.ab = ifelse(pitchNumber == max(pitchNumber), "true", "false"), 
                    last.pitch.of.ab = factor(last.pitch.of.ab)) %>% 
      ungroup()
    
    pbp <- dplyr::bind_rows(stats_api_live_empty_df, pbp) # is this going to throw errors? Object stats_api_live_empty_df not yet created
    
    check_home_level <- pbp %>% dplyr::distinct(home_level_id) %>% dplyr::pull()
    
    pbp <- pbp %>% dplyr::rename(count.balls.start   = count.balls.x, 
                                 count.strikes.start = count.strikes.x, count.outs.start = count.outs.x, 
                                 count.balls.end     = count.balls.y, count.strikes.end = count.strikes.y, 
                                 count.outs.end      = count.outs.y)
    return(pbp)
  }
  
  
  # this gives you a list, one list object per game
  dat <- map(gids, f)
  
  
  # --- unlist and create data frame
  require(plyr)
  df <- rbind.fill(lapply(dat,function(y){as.data.frame(y,stringsAsFactors=FALSE)}))
  
  saveRDS(df, file = "statcast2020.rds", compress=FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  