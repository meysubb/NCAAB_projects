options(stringsAsFactors = FALSE)
options(digits=8)
year_id <- "12480"

library(dplyr)
conf_team_dat <- readRDS("conferences/conf_team.RDS")
agg.team <- readRDS("data/agg_team.RDS")
agg.team <- inner_join(agg.team,conf_team_dat)
ind.team <- readRDS("data/ind_team.RDS")

#points/poss = points/(FGA + 0.475 x FTA)   x   (FGA + 0.475 x FTA)/poss
#possessions ~ FGA + 0.475 * FTA - ORB + TO
#ORTG = 100*(PTS Scored)/POSS
#DRTG = 100*(PTS Allowed)/POSS
ind.team <- ind.team %>% mutate(
  possessions = round(team_fga + 0.475 * team_fta - team_offreb + team_to),
  points_per_poss = team_pts/(team_fga + 0.475 * team_fta) * (team_fga + 0.475 * team_fta)/possessions,
  d_points_per_poss = opp_pts/(opp_fga + 0.475 * opp_fta) * (opp_fga + 0.475 * opp_fta)/possessions,
  ppp_diff = points_per_poss - d_points_per_poss,
  ORTG = 100 * (team_pts)/possessions,
  DRTG = 100 * (opp_pts/possessions)
) 

### Add advanced shooting stats
agg.player <- readRDS("data/agg_player.RDS")
ind.player <- readRDS("data/ind_player.RDS")

ind.player$team_id <- gsub("12480","",ind.player$team_id)
### Create the Team Minutes Played stat 
### Go across the ind.player data frame, group by team_name, game_id 
### sum the minutes across all players
### inner join the summary with agg player data frame
detach("package:reshape", unload=TRUE)
tm_mins_calc <- ind.player %>% group_by(team_id,game) %>% 
  mutate(tm_mins = sum(minutes,na.rm = TRUE)) %>% select(team_id,game,tm_mins) %>% distinct(game, .keep_all = TRUE) %>% rename(game_id = game)

ind.team <- inner_join(ind.team,tm_mins_calc)

### Summarize the stats and add to the overall team dataframe
#TS% = 0.5 x points/(FGA + 0.475 x FTA)
#eFG% (FG + 0.5 * 3P) / FGA.
# pythagorean wins 
summary <- ind.team %>% group_by(team_name) %>% 
  summarize(possessions = sum(possessions),
            o_ppp = mean(points_per_poss),
            d_ppp = mean(d_points_per_poss),
            ORTG_sum = mean(ORTG),
            DRTG_sum = mean(DRTG))
agg.team <- inner_join(agg.team,summary)

agg.team <- agg.team %>% mutate(
  diff_ppp = o_ppp - d_ppp,
  ts_pct = 0.5 * team_pts/(team_fga + 0.475 * team_fta),
  efg_pct = (team_fgm + 0.5 * team_three_fgm) / team_fga,
  pyth_win = team_pts^14 / (team_pts^14 + opp_team_pts^14),
  three_par = team_three_fga/team_fga
) 


#TS% = 0.5 x points/(FGA + 0.475 x FTA)
#eFG% (FG + 0.5 * 3P) / FGA.
agg.player <- agg.player %>% mutate(
  ts_pct = (0.5 * pts)/(fga + 0.475 * fta),
  efg_pct = (fgm + 0.5 * three_fgm)/fga,
  three_par = three_fga/fga
)


### Rebound rate
### TRB rate = 100 * (TRB * (Tm MP / 5)) / (MP * (Tm TRB + Opp TRB))
### Ast rate = 100 * AST / (((MP / (Tm MP / 5)) * Tm FG) - FG).
### Block rate = 100 * (BLK * (Tm MP / 5)) / (MP * (Opp FGA - Opp 3PA)).
### Tov rate = 100 * TOV / (FGA + 0.475 * FTA + TOV) 
### Usage rate = 100 * ((FGA + 0.44 * FTA + TOV) * (Tm MP / 5)) / (MP * (Tm FGA + 0.44 * Tm FTA + Tm TOV)
### Tov rate didn't work
### finished
team_dat <- ind.team %>% select(game_id,team_fgm,team_fga,team_fta,team_to,team_name,team_totreb,opp_totreb,tm_mins,opp_fga,opp_three_fga) %>% rename(game = game_id)
player_dat <- ind.player %>% select(minutes,game,player_name,team_name,totreb,ast,fgm,fga,fta,blk,to)

ind.player_adv <- inner_join(player_dat,team_dat) %>% mutate(fgm = as.numeric(fgm)) 
ind.player_adv[is.na(ind.player_adv)] <- 0

ind.player_adv <- ind.player_adv %>% mutate(
  reb_rate = 100 * ((totreb * tm_mins/5)/ (minutes * (team_totreb + opp_totreb))),
  ast_rate = 100 * (ast / (((minutes/ (tm_mins/5)) * team_fgm) - fgm)),
  blk_rate = 100 * (blk * (tm_mins/5)) / (minutes * (opp_fga - opp_three_fga)),
  usg_rate = 100 * ( ((fga + 0.475 * fta + to) * (tm_mins/5)) / (minutes * (team_fga + 0.475 * team_fta + team_to)))
#  to_rate =  100 * (to / (fga + 0.475 * fta + to))
)
ind.player_adv[is.na(ind.player_adv)] <- 0
ind.player_adv[ind.player_adv$usg_rate == Inf,] <- 0

agg.player_adv <- ind.player_adv %>% group_by(team_name,player_name) %>% summarize(
  reb_rate = mean(reb_rate,na.rm = TRUE),
  ast_rate = mean(ast_rate,na.rm = TRUE),
  blk_rate = mean(blk_rate,na.rm = TRUE),
  usg_rate = mean(usg_rate,na.rm = TRUE)
#  to_rate = mean(to_rate,na.rm = TRUE)
)

agg.player <- inner_join(agg.player,agg.player_adv)


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

colnames(agg.team)[c(82:83,85)] <- paste0("team_",colnames(agg.team)[c(82:83,85)])
agg.player <- round_df(agg.player,2)
ind.team <- round_df(ind.team,2)
ind.player$fgm <- as.numeric(ind.player$fgm)
ind.player[,7:22][is.na(ind.player[,7:22])] <- 0
ind.player <- round_df(ind.player,2)


## Save agg.team
saveRDS(agg.team,file="agg_team.RDS")
## Save agg.player
saveRDS(agg.player,file="agg_player.RDS")
## Save ind.team
saveRDS(ind.team,file="ind_team.RDS")
## Save ind.player 
saveRDS(ind.player,file="ind_player.RDS")
