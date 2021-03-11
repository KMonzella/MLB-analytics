### Evan Boyd
### University of Chicago Capstone
### enboyd@uchicago.edu


library(baseballr)
library(dplyr)

###Load the data here
#statcast2020 <- readRDS("statcast2020_full.rds")

#Only pitch types
pitcher_tendencies <- function(dat, pitcher_id = 0, confidence = 0.5, 
                               addCount = FALSE, addOuts = FALSE) {
  
  #If pitcher is specified, filter only his data and return if there is no data
  if(pitcher_id != 0) {
    dat <- dat %>% 
      filter(matchup.pitcher.id == pitcher_id)
    
    if(nrow(dat) == 0) {
      return("No data available for that player. Please try again or display all.")
    }
  }
  
  if(confidence >= 1 | confidence <= 0) {
    return("Confidence Level out of range. Please choose a number between 0 and 1")
  } else {
    dat <- dat %>% 
      filter(pitchData.typeConfidence >= confidence)
  }
  
  ##No matter what, we are looking at pitches only
  dat <- dat %>% 
    filter(type == "pitch")
  
  #Create a column that will look at the count, if asked
  if(addCount) {
    
    for(i in 1:nrow(dat)) {
      
      if(dat$details.isStrike[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i], dat$count.strikes.start[i] - 1, sep = "-")
      } else if (dat$details.isBall[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i] - 1, dat$count.strikes.start[i], sep = "-")
      } else if(dat$details.isInPlay[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i], dat$count.strikes.start[i], sep = "-")
      } else {
        dat$Count[i] <- paste0("Pitch Not a Ball or Strike. Original Count: ",
                               dat$count.balls.start[i], "-",
                               dat$count.strikes.start[i] )
      }
      
    }
  }
  
  ##Calculate pitches seen
  
  pitches <- dat %>% 
    dplyr::rename(pitcher = matchup.pitcher.id,
                  player_name = matchup.pitcher.fullName,
                  events = result.event,
                  outs = count.outs.start,
                  pitch_type = details.type.code) %>% 
    filter(!is.na(pitch_type), type == "pitch") %>% 
    mutate(all_fastballs = case_when(pitch_type %in% c("SI", "FT", "FF","FC","FO") ~ 1, TRUE ~ 0),
           offspeed = case_when(pitch_type %in% c("CH", "EP", "KN","FS") ~ 1, TRUE ~ 0),
           breaking = case_when(pitch_type %in% c("CU", "KC", "SC", "SL", "CS") ~ 1, TRUE ~ 0),
           four_seam = ifelse(pitch_type == "FF", 1, 0),
           two_seam = ifelse(pitch_type == "FT", 1, 0),
           changeup = ifelse(pitch_type == "CH", 1, 0),
           slow_curve = ifelse(pitch_type == "CS", 1, 0),
           curve = ifelse(pitch_type == "CU", 1, 0),
           cutter = ifelse(pitch_type == "FC", 1, 0),
           eephus = ifelse(pitch_type == "EP", 1, 0),
           forkball = ifelse(pitch_type == "FO", 1, 0),
           knuckleball = ifelse(pitch_type == "KN", 1, 0),
           knuckle_curve = ifelse(pitch_type == "KC", 1, 0),
           screwball = ifelse(pitch_type == "SC", 1, 0),
           sinker = ifelse(pitch_type == "SI", 1, 0),
           slider = ifelse(pitch_type == "SL", 1, 0),
           splitter = ifelse(pitch_type == "FS", 1, 0),
           unknown = ifelse(is.na(pitch_type), 1, 0))
  
  
  
  #These are still the statcast abbreviations, which look good on Bill Petti
  #But in the first set of data are missing FT (Two-seam) and FO (forkball)
  #Will need to check those two with full data
  
  if(addCount) {
    
    if(addOuts) {
      pitches <- pitches %>%
        group_by(pitcher, Count, outs) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         curveball = sum(curve)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    } else {
      pitches <- pitches %>%
        group_by(pitcher, Count) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         curveball = sum(curve)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    }
    
  } else {
    
    if(addOuts) {
      pitches <- pitches %>%
        group_by(pitcher, outs) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         curveball = sum(curve)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    } else { pitches <- pitches %>%
      group_by(pitcher) %>% 
      dplyr::summarise(player = max(as.character(player_name)), 
                       pitches = n(),
                       fastballs = sum(all_fastballs)/n(),
                       offspeed = sum(offspeed)/n(),
                       breaking_ball = sum(breaking)/n(),
                       fs_fb = sum(four_seam)/n(),
                       ts_fb = sum(two_seam)/n(),
                       change = sum(changeup)/n(),
                       curveball = sum(curve)/n(),
                       slow_curve = sum(slow_curve)/n(),
                       cutter = sum(cutter)/n(),
                       eephus = sum(eephus)/n(),
                       fork = sum(forkball)/n(),
                       knuckle = sum(knuckleball)/n(),
                       knuckle_curve = sum(knuckle_curve)/n(),
                       screw = sum(screwball)/n(),
                       sink = sum(sinker)/n(),
                       slider = sum(slider)/n(),
                       split = sum(splitter)/n(),
                       .groups = "drop"
      ) %>% 
      arrange(desc(pitches))}
  }
  output <- pitches
  
  return(output)
}


test <- pitcher_tendencies(statcast2020, pitcher_id = 543294)

pitcher_tendencies(statcast2020, pitcher_id = 543294, addCount = FALSE)
pitcher_tendencies(statcast2020, pitcher_id = 543294, addCount = TRUE)
pitcher_tendencies(statcast2020, pitcher_id = 543294, addCount = TRUE, addOuts = TRUE)
pitcher_tendencies(statcast2020, pitcher_id = 543294, addOuts = TRUE)



batter_tendencies <- function(dat, batter_id = 0, confidence = 0.5, 
                              addCount = FALSE, addOuts = FALSE) {
  
  #If batter is specified, filter only his data and return if there is no data
  if(batter_id != 0) {
    dat <- dat %>% 
      filter(matchup.batter.id == batter_id)
    
    if(nrow(dat) == 0) {
      return("No data available for that player. Please try again or display all.")
    }
  }
  
  if(confidence >= 1 | confidence <= 0) {
    return("Confidence Level out of range. Please choose a number between 0 and 1")
  } else {
    dat <- dat %>% 
      filter(pitchData.typeConfidence >= confidence)
  }
  
  ##No matter what, we are looking at pitches only
  dat <- dat %>% 
    filter(type == "pitch")
  
  if(addCount) {
    
    for(i in 1:nrow(dat)) {
      
      if(dat$details.isStrike[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i], dat$count.strikes.start[i] - 1, sep = "-")
      } else if (dat$details.isBall[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i] - 1, dat$count.strikes.start[i], sep = "-")
      } else if(dat$details.isInPlay[i]) {
        dat$Count[i] <- paste(dat$count.balls.start[i], dat$count.strikes.start[i], sep = "-")
      } else {
        dat$Count[i] <- paste0("Pitch Not a Ball or Strike. Original Count: ",
                               dat$count.balls.start[i], "-",
                               dat$count.strikes.start[i] )
      }
      
    }
  }

  ##Calculate pitches seen
  #These are still the statcast abbreviations, which look good on Bill Petti
  #But in the first set of data are missing FT (Two-seam) and FO (forkball)
  #Will need to check those two with full data
  
  pitches <- dat %>% 
    dplyr::rename(batter = matchup.batter.id,
                  player_name = matchup.batter.fullName,
                  events = result.event,
                  outs = count.outs.start,
                  pitch_type = details.type.code) %>% 
    filter(!is.na(pitch_type)) %>% 
    mutate(all_fastballs = case_when(pitch_type %in% c("SI", "FT", "FF","FC","FO") ~ 1, TRUE ~ 0),
           offspeed = case_when(pitch_type %in% c("CH", "EP", "KN","FS") ~ 1, TRUE ~ 0),
           breaking = case_when(pitch_type %in% c("CU", "KC", "SC", "SL", "CS") ~ 1, TRUE ~ 0),
           four_seam = ifelse(pitch_type == "FF", 1, 0),
           two_seam = ifelse(pitch_type == "FT", 1, 0), #Apparently none in Bill Petti database
           changeup = ifelse(pitch_type == "CH", 1, 0),
           slow_curve = ifelse(pitch_type == "CS", 1, 0),
           curve = ifelse(pitch_type == "CU", 1, 0),
           cutter = ifelse(pitch_type == "FC", 1, 0),
           eephus = ifelse(pitch_type == "EP", 1, 0),
           forkball = ifelse(pitch_type == "FO", 1, 0),
           knuckleball = ifelse(pitch_type == "KN", 1, 0),
           knuckle_curve = ifelse(pitch_type == "KC", 1, 0),
           screwball = ifelse(pitch_type == "SC", 1, 0),
           sinker = ifelse(pitch_type == "SI", 1, 0),
           slider = ifelse(pitch_type == "SL", 1, 0),
           splitter = ifelse(pitch_type == "FS", 1, 0),
           unknown = ifelse(is.na(pitch_type), 1, 0)
    )
  
  if(addCount) {
    
    if(addOuts) {
      pitches <- pitches %>% 
        group_by(batter, Count, outs) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         curveball = sum(curve)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    } else {
      
      pitches <- pitches %>% 
        group_by(batter, Count) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         curveball = sum(curve)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    }
  
  } else {
    
    if(addOuts) {
      pitches <- pitches %>% 
        group_by(batter, outs) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         curveball = sum(curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    } else {
      pitches <- pitches %>% 
        group_by(batter) %>% 
        dplyr::summarise(player = max(as.character(player_name)), 
                         pitches = n(),
                         fastballs = sum(all_fastballs)/n(),
                         offspeed = sum(offspeed)/n(),
                         breaking_ball = sum(breaking)/n(),
                         fs_fb = sum(four_seam)/n(),
                         ts_fb = sum(two_seam)/n(),
                         change = sum(changeup)/n(),
                         slow_curve = sum(slow_curve)/n(),
                         curveball = sum(curve)/n(),
                         cutter = sum(cutter)/n(),
                         eephus = sum(eephus)/n(),
                         fork = sum(forkball)/n(),
                         knuckle = sum(knuckleball)/n(),
                         knuckle_curve = sum(knuckle_curve)/n(),
                         screw = sum(screwball)/n(),
                         sink = sum(sinker)/n(),
                         slider = sum(slider)/n(),
                         split = sum(splitter)/n(),
                         .groups = "drop"
        ) %>% 
        arrange(desc(pitches))
    }
    
  }
  output <- pitches
  
  return(output)
}
# 
# batter_tendencies(statcast2020, batter_id = 641313 , confidence = 0.005)
# 
# batter_tendencies(statcast2020, batter_id = 641313 , 
#                   confidence = 0.005, addCount = TRUE)
# 
# 
# batter_tendencies(statcast2020, batter_id = 641313 , 
#                   confidence = 0.005, addOuts = TRUE)
# 
# 
# batter_tendencies(statcast2020, batter_id = 641313 , 
#                   confidence = 0.005, addCount = TRUE, addOuts= TRUE)
