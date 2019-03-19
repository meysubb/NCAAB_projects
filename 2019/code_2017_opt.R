library(lubridate)
library(dplyr)

source("funcs.R")  
options(stringsAsFactors = FALSE)
options(digits=8)
year_id <- "14300"
### New Updated Code 
### Focusing on Optimization 

## Aggregate Player Data
agg.player <- read.csv(file="scraped_data/summary_player_data.tsv", sep="\t", header=TRUE, na.strings="?",stringsAsFactors = F)
start.time <- Sys.time()
## Change Team and Player Name to Characters
char_cols <- c("player_name","team_name")
agg.player[char_cols] <- lapply(agg.player[char_cols], as.character)
## Convery Position and year to Factors
fac_cols <- c("pos","year")
agg.player[fac_cols] <- lapply(agg.player[fac_cols], as.factor)
## Convery Height and Minutes
agg.player$height <- sapply(strsplit(as.character(agg.player$height), "-"), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
## Add new Assist Columns 
agg.player$trips <- 0
colnames(agg.player)[12:34] <- colnames(agg.player)[11:33]
agg.player <- agg.player[,-11] 

agg.player <- agg.player %>% 
  mutate(
    mins = ms(minutes),
    minutes = as.numeric(mins)/60,
    played = as.numeric(played),
    astavg = ast/played,
    ast_to = ifelse(to==0,0,ast/to)
  ) %>% select(-mins)

# replace's NA with 0, except for factor columns i.e Pos
agg.player[is.na(agg.player)] <- 0 

# Load aggregate team data
# Remove strings as factors
agg.team <- read.csv(file="scraped_data/summary_team_data.tsv", sep="\t", header=TRUE, na.strings="?",stringsAsFactors = FALSE)

agg.team$opp_trips <- 0
colnames(agg.team)[4:49] <- colnames(agg.team)[3:48]
agg.team <- agg.team[,-3] 

# Change Team to Character
agg.team <- agg.team %>% mutate(
  team_name = as.character(team_name),
  mins = ms(team_minutes),
  team_minutes = as.numeric(mins)/60,
  opp_mins = ms(opp_team_minutes),
  opp_team_minutes = as.numeric(minute(opp_mins)*60 + seconds(opp_mins))
) %>% select(-mins,-opp_mins)

# Remove All Commas
com_cols <- names(which(apply(agg.team, 2, function(x) any(grepl(",", x)))))
agg.team[com_cols] <- apply(agg.team[com_cols], 2, function(x) as.numeric(gsub(",", "", x)))
agg.team$team_ast_to <- agg.team$team_ast/agg.team$team_to
agg.team[is.na(agg.team)] <- 0 
agg.team$team_id <- rownames(agg.team)

# Load Individual Game Data
ind.game <- read.csv(file="scraped_data/game_data.tsv", sep="\t", header=TRUE, row.names=1, na.strings="?",stringsAsFactors = FALSE)
ind.game$temp <- 0
colnames(ind.game)[5:19] <- colnames(ind.game)[4:18]
colnames(ind.game)[23:37] <- colnames(ind.game)[22:36]
ind.game <- ind.game[,-c(4,22)]


## Change Team Names to Characters
char_cols <- c("home_team_name","away_team_name")
ind.game[char_cols] <- lapply(ind.game[char_cols], as.character)
## Make minutes numeric 
ind.game <- ind.game %>% mutate(
  game_date = as.Date(game_date,format='%m/%d/%Y'),
  home_mins = ms(home_team_minutes),
  home_team_minutes = as.numeric(home_mins)/60,
  away_mins = ms(away_team_minutes),
  awaay_team_minutes = as.numeric(away_mins)/60,
  home_team_id = create_team_id(home_team_id,year_id),
  away_team_id = create_team_id(away_team_id,year_id)
) %>% select(-home_mins,-away_mins) %>% rownames_to_column(var='game_id')


# Load Individual Player Data
ind.player <- read.csv(file="scraped_data/player_data.tsv", sep="\t", header=TRUE, row.names=NULL, na.strings="?")
colnames(ind.player)[8:22]  <- colnames(ind.player)[7:21] 
ind.player <- ind.player[,-7]

## Change Team and Player Name to Characters
char_cols <- c("player_name","team_name")
ind.player[char_cols] <- lapply(ind.player[char_cols], as.character)
## Add factors for position
ind.player <- ind.player %>% mutate(
  pos = factor(pos, c("G", "F", "C")),
  mins = ms(minutes),
  minutes = as.numeric(mins)/60,
  game_date = as.Date(game_date,format='%m/%d/%Y'),
  team_id = create_team_id(team_id,year_id)
) %>% select(-mins)


# Load Individual Team Data
ind.team <- read.csv(file="scraped_data/team_data.tsv", sep="\t", header=TRUE, row.names=NULL, na.strings="?")
colnames(ind.team)[7:21] <- colnames(ind.team)[6:20]
ind.team <- ind.team[,-6]

ind.team <- ind.team %>% mutate(
  team_name = as.character(team_name),
  game_date = as.Date(game_date,format = '%m/%d/%Y'),
  team_minutes = as.numeric(ms(team_minutes))/60
)

if(colnames(ind.team)[11] == "team_ft"){
  colnames(ind.team)[11] = "team_ftm"
}

# Error Checking 
check_cols <- c("opp_team_fgpct","opp_team_three_fgpct","opp_team_ftpct","opp_team_ptsavg","opp_team_rebavg")
err_lst <- lapply(seq(along=check_cols),function(x){
  temp_df <- cbind(find_errors(agg.team,check_cols[x],10),check_cols[x])
})
# Set matching column names
library(dplyr)
library(plyr)
colnames <- c("team_name", "value", "stat")
err_lst <- lapply(err_lst, setNames, colnames)
err_df <- ldply(err_lst)
err_df <- err_df %>%
  filter(value > 100.00)
################# TAKE OUR EXISTING DATA, ADD TO IT, AND CREATE NEW DATAFRAMES

##### Add a few pieces of data to each game
# Point differential for each game
# Field goal percentage
# Three-point percentage
# Free-throw percentage
detach("package:plyr", unload=TRUE)
library(tidyr)

ind.game <- ind.game %>% mutate(
  ptsdiff = home_team_pts - away_team_pts,
  away_team_fgpct = away_team_fgm/away_team_fga,
  home_team_fgpct = home_team_fgm/home_team_fga,
  away_team_three_fgpct = away_team_three_fgm/away_team_three_fga,
  home_team_three_fgpct = home_team_three_fgm/home_team_three_fga,
  away_team_ftpct = away_team_ft/away_team_fta,
  home_team_ftpct = home_team_ft/home_team_fta
)


##### Create a list with our basic stats
basicgamestats <- c("fgm", "fga", "three_fgm", "three_fga", "ft", "fta", "pts", "ptsavg", "offreb", "defreb", "totreb", "rebavg", "ast", "to", "stl", "blk", "dbldbl", "trpdbl","fgpct", "three_fgpct", "ftpct")
basicgamestats_team <- basicgamestats

## Add Player Fouls to AGG player DF. 

ind.player_sep <- ind.player %>%  group_by(player_name)  %>% summarise(PF = sum(fouls,na.rm=TRUE))
test <- full_join(ind.player_sep,agg.player)
test <- test %>% drop_na(team_id) %>% select(player_name,PF)
agg.player$PF <- test$PF[match(agg.player$player_name,test$player_name)]
  
##### Calculate additional team data
agg.team$team_name <- trimws(agg.team$team_name)

## Calculate PTS-Diff (Home/Away/General)
ind.game_avg <- ind.game %>% group_by(home_team_name) %>% summarise(avg_ptsdiff = mean(ptsdiff,na.rm=TRUE))
colnames(ind.game_avg)[1] <- "team_name"
agg.team$team_home_avg_ptsdiff <- ind.game_avg$avg_ptsdiff[match(agg.team$team_name,ind.game_avg$team_name)]

ind.game_avg <- ind.game %>% group_by(away_team_name) %>% summarise(avg_ptsdiff = mean(-1 * ptsdiff,na.rm=TRUE))
colnames(ind.game_avg)[1] <- "team_name"
agg.team$team_away_avg_ptsdiff <- ind.game_avg$avg_ptsdiff[match(agg.team$team_name,ind.game_avg$team_name)]

agg.team$team_avg_ptsdiff <-  with(agg.team,(team_home_avg_ptsdiff + team_away_avg_ptsdiff)/2)

## Calculate Wins/Losses
## Check this over, weird results for LaSalle and other teams
ind.game_home <- ind.game %>% group_by(home_team_name) %>% summarise(wins = length(which(ptsdiff > 0)),
                                                                     losses = length(which(ptsdiff < 0)))
colnames(ind.game_home)[1] <- "team_name"
agg.team$team_home_wins <- ind.game_home$wins[match(agg.team$team_name,ind.game_home$team_name)] 
agg.team$team_home_losses <- ind.game_home$losses[match(agg.team$team_name,ind.game_home$team_name)] 

ind.game_away <- ind.game %>% group_by(away_team_name) %>% summarise(wins = length(which(ptsdiff < 0)),
                                                                     losses = length(which(ptsdiff > 0)))
colnames(ind.game_away)[1] <- "team_name"
agg.team$team_away_wins <- ind.game_away$wins[match(agg.team$team_name,ind.game_away$team_name)] 
agg.team$team_away_losses <- ind.game_away$losses[match(agg.team$team_name,ind.game_away$team_name)] 

# Overall 
agg.team$team_wins <- with(agg.team,team_home_wins + team_away_wins)
agg.team$team_losses <- with(agg.team,team_home_losses + team_away_losses)
agg.team$winpct <- with(agg.team, team_wins/(team_wins + team_losses))

# Guard, Forwards, Center points
pts_per_pos <- agg.player %>% group_by(team_name,pos) %>% summarise(points_pos = sum(pts))
levels(pts_per_pos$pos)[1] <- "empty"
refined <- pts_per_pos[ !grepl("empty", pts_per_pos$pos) , ]
final_pts_per_pos <- refined %>% spread(pos, points_pos, fill = NA, convert = FALSE)
final_pts_per_pos[is.na(final_pts_per_pos)] <- 0
colnames(final_pts_per_pos)[2:4] <- paste0(colnames(final_pts_per_pos)[2:4],"_points")
### Add data 
agg.team <- full_join(agg.team,final_pts_per_pos)

# G,F,C average height. 

ht_per_pos <- agg.player %>% group_by(team_name,pos) %>% summarise(height = mean(height))
levels(ht_per_pos$pos)[1] <- "empty"
refined <- ht_per_pos[ !grepl("empty", ht_per_pos$pos) , ]
final_ht_per_pos <- refined %>% spread(pos, height, fill = NA, convert = FALSE)
final_ht_per_pos[is.na(final_ht_per_pos)] <- 0
colnames(final_ht_per_pos)[2:4] <- paste0(colnames(final_ht_per_pos)[2:4],"_avg_height")
### Add data 
agg.team <- full_join(agg.team,final_ht_per_pos)

rm(refined)
rm(test)

# Add a few more elements to our vector of variables
basicgamestatsaddsingle <- c("home_avg_ptsdiff","away_avg_ptsdiff","avg_ptsdiff","home_wins","away_wins","wins","home_losses","away_losses","losses")
basicgamestats_team <- append(basicgamestats_team, basicgamestatsaddsingle)

home_away_vars_to_populate <- paste0("team_",basicgamestats_team)
away_vars <- paste0("away_season_", home_away_vars_to_populate)
home_vars <- paste0("home_season_", home_away_vars_to_populate)
### Try a for each loopl
library(parallel)
library(doSNOW)
nocores <- detectCores() - 1
cl <- makeCluster(nocores)
registerDoSNOW(cl)
pb <- txtProgressBar(max = 32, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


away_var_list <- foreach(i=1:30,.combine=data.frame, .packages = "dplyr",.options.snow = opts) %dopar% {
  gen_var_name <- home_away_vars_to_populate[i]
  #away_command <- away_vars[i]
  sapply(seq(1:length(ind.game$game_date)),function(x){
    #gen_game_id <- ind.game$game_id[x]
    gen_away_team_id <- as.character(ind.game$away_team_name[x])
    tm_sub <- agg.team %>% filter(team_name == gen_away_team_id)
    ifelse(nrow(tm_sub) == 0,-999,tm_sub[,gen_var_name])
  })
}
close(pb)

pb <- txtProgressBar(max = 30, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


home_var_list <- foreach(i=1:30,.combine=data.frame, .packages = "dplyr",.options.snow = opts) %dopar% {
  gen_var_name <- home_away_vars_to_populate[i]
  #away_command <- away_vars[i]
  sapply(seq(1:length(ind.game$game_date)),function(x){
    #gen_game_id <- ind.game$game_id[x]
    gen_home_team_id <- as.character(ind.game$home_team_name[x])
    tm_sub <- agg.team %>% filter(team_name == gen_home_team_id)
    ifelse(nrow(tm_sub) == 0,-999,tm_sub[,gen_var_name])
  })
}
stopCluster(cl)
## Transform List to DF
colnames(away_var_list) <- away_vars
colnames(home_var_list) <- home_vars

var_list_pop <- cbind(away_var_list,home_var_list)
rm(away_var_list,home_var_list)
 
ind.game <- cbind(ind.game,var_list_pop)

rm(var_list_pop)
rm(opts)
rm(pb)

##### Create a new team-level data frame that has individual game stats and looks like: Team Stats, Opponent Stats (Note: Each game will thus have two entries!)
# First, the away team
vec_team_away <- ind.game %>% select(team_id = away_team_id,team_name = away_team_name,game_id,game_date,neutral_site,opp_team_id = home_team_id,opp_team_name = home_team_name)
vec_team_away$home <- 0
basicgamestats_team_2 <- basicgamestats_team[1:24]
away_names <- reshape_team_lvl_data(ind.game,basicgamestats_team_2,"team_","away_team_")
opp_names <- reshape_team_lvl_data(ind.game,basicgamestats_team_2,"opp_","home_team_")
away_sea_names <- reshape_team_lvl_data(ind.game,basicgamestats_team,"team_season_","away_season_team_")
opp_sea_names <- reshape_team_lvl_data(ind.game,basicgamestats_team,"opp_season_","home_season_team_")

away <- inner_join(away_names,away_sea_names,by=c("team_game_id"="game_id"))
opp <- inner_join(opp_names,opp_sea_names,by=c("opp_game_id"="game_id"))
away_opp.game <- inner_join(away,opp,by=c("team_game_id"="opp_game_id"))
vec_team_away <- inner_join(vec_team_away,away_opp.game,by=c("game_id"="team_game_id"))
vec_team_away$ptsdiff <- -1 * ind.game$ptsdiff


vec_team_home <- ind.game %>% select(team_id = home_team_id,team_name = home_team_name,game_id,game_date,neutral_site,opp_team_id = away_team_id,opp_team_name = away_team_name)
vec_team_home$home <- 1
home_names <- reshape_team_lvl_data(ind.game,basicgamestats_team_2,"team_","home_team_")
opp_names <- reshape_team_lvl_data(ind.game,basicgamestats_team_2,"opp_","away_team_")
home_sea_names <- reshape_team_lvl_data(ind.game,basicgamestats_team,"team_season_","home_season_team_")
opp_sea_names <- reshape_team_lvl_data(ind.game,basicgamestats_team,"opp_season_","away_season_team_")

home <- inner_join(home_names,home_sea_names,by=c("team_game_id"="game_id"))
opp <- inner_join(opp_names,opp_sea_names,by=c("opp_game_id"="game_id"))
home_opp.game <- inner_join(home,opp,by=c("team_game_id" = "opp_game_id"))
vec_team_home <- inner_join(vec_team_home,home_opp.game,by=c("game_id"="team_game_id"))
vec_team_home$ptsdiff <- ind.game$ptsdiff

ind.team <- rbind(vec_team_away,vec_team_home)

rm(list = ls(pattern = "\\opp"))
rm(list = ls(pattern = "\\home"))
rm(list = ls(pattern = "\\away"))

##### Add in opponents' winning percentage, etc. (all stats from basicgamestatsaddsingle, but for opponents)
# Create a vector with each team
# Ensure column names are set properly 
# .x -> home team
# .y -> opp team
colnames(ind.team)[45:48] <- gsub("\\b.x","",colnames(ind.team)[45:48]) 
colnames(ind.team)[87:88] <- paste0("opp_",gsub("\\b.y","",colnames(ind.team)[87:88]))

## Look for opponent stats
home_agg_team_stats <- ind.team %>% group_by(team_id,opp_team_id) %>% filter(home == 1) %>% 
  summarise(opp_home_avg_ptsdiff = mean(-1 * ptsdiff),
            opp_home_wins = length(which(ptsdiff>0)),
            opp_home_losses = length(which(ptsdiff<0))) 
## Aggregate team stats
home_agg_team_stats <- home_agg_team_stats %>% group_by(team_id) %>% 
  summarise(opp_home_avg_ptsdiff = mean(opp_home_avg_ptsdiff),
            opp_home_wins = sum(opp_home_wins),
            opp_home_losses = sum(opp_home_losses))
## Look for opponent stats
away_agg_team_stats <- ind.team %>% group_by(team_id,opp_team_id) %>% filter(home == 0) %>% 
  summarise(opp_away_avg_ptsdiff = mean(-1 * ptsdiff),
            opp_away_wins = length(which(ptsdiff>0)),
            opp_away_losses = length(which(ptsdiff<0))) 
## Aggregate team stats
away_agg_team_stats <- away_agg_team_stats %>% group_by(team_id) %>% 
  summarise(opp_away_avg_ptsdiff = mean(opp_away_avg_ptsdiff),
            opp_away_wins = sum(opp_away_wins),
            opp_away_losses = sum(opp_away_losses))
## Look for opponent stats
agg_team_stats <- ind.team %>% group_by(team_id,opp_team_id) %>% 
  summarise(opp_avg_ptsdiff = mean(-1 * ptsdiff),
            opp_wins = length(which(ptsdiff>0)),
            opp_losses = length(which(ptsdiff<0))) 
## Aggregate team stats
agg_team_stats <- agg_team_stats %>% group_by(team_id) %>% 
  summarise(opp_avg_ptsdiff = mean(opp_avg_ptsdiff),
            opp_wins = sum(opp_wins),
            opp_losses = sum(opp_losses)) 

agg_team_stats_comb <- inner_join(home_agg_team_stats,away_agg_team_stats)
agg_team_stats_comb <- inner_join(agg_team_stats_comb,agg_team_stats)

agg.team <- inner_join(agg.team,agg_team_stats_comb)

rm(list = ls(pattern = "agg_team_stats"))


## Prepare away opponent  data to be appended to ind.game from agg.team
sel_cols <- c("team_id",colnames(agg.team)[67:75])
opp_away <- agg.team %>% select_(.dots=sel_cols) 
opp_home <- opp_away
colnames(opp_away)[2:10] <- paste0("away_season_",colnames(opp_away)[2:10])
colnames(opp_away)[1] <- "away_team_id"
## Prepare home opponent  data to be appended to ind.game from agg.team
colnames(opp_home)[2:10] <- paste0("home_season_",colnames(opp_home)[2:10])
colnames(opp_home)[1] <- "home_team_id"


ind.game <- inner_join(ind.game,opp_away)
ind.game <- inner_join(ind.game,opp_home)

## Save agg.team
saveRDS(agg.team,file="agg_team.RDS")
## Save agg.player
saveRDS(agg.player,file="agg_player.RDS")
## Save ind.team
saveRDS(ind.team,file="ind_team.RDS")
## Save ind.player 
saveRDS(ind.player,file="ind_player.RDS")

write.csv(agg.team,"raw_data/agg_team.csv")
write.csv(agg.player,"raw_data/agg_player.csv")
write.csv(ind.team,"raw_data/ind_team.csv")
write.csv(ind.player,"raw_data/ind_player.csv")
