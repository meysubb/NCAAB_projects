library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)
library(plyr)
# Make sure to load all relevant CSV files
## Save agg.team
agg.team <- readRDS(file="agg_team.RDS")
## Save agg.player
agg.player<- readRDS(file="agg_player.RDS")
## Save ind.team
ind.team <- readRDS(file="ind_team.RDS")
## Load ind.player
ind.player <- readRDS(file="ind_player.RDS")
# Clean up the data in vis.R


## Clean up Data
agg.team$team_three_fga <- gsub(",","",agg.team$team_three_fga)
agg.team$team_three_fga <- as.numeric(agg.team$team_three_fga)
agg.team$team_pts <- as.numeric(agg.team$team_pts)
agg.team$opp_team_fgm <- gsub(",","",agg.team$opp_team_fgm)
agg.team$opp_team_fgm <- as.numeric(agg.team$opp_team_fgm)

# FT-PCT
ind.player$ftpct <- ind.player$ft / ind.player$fta
ind.player$fgpct <- ind.player$fgm / ind.player$fga
ind.player$fgpct[is.na(ind.player$fgpct)] <- 0
ind.player$ftpct[is.na(ind.player$ftpct)] <- 0

# Make Tourney Data
# Create a vector with all of the 68 teams in the NCAA Tournament
# Need to edit this on Selection Sunday 
tourney_teams <- read.csv(file="tourney_teams.csv")
tourneyteams <- as.character(tourney_teams$Team.ID)

tourney_team_agg_stats <- agg.team[tourneyteams, ]
team_names <- unique(sort(tourney_team_agg_stats$team_name))
tourney_team_ind_stats <- subset(ind.team, thisteam_team_id %in% tourneyteams)
tourney_player_ind_stats <- subset(ind.player, team_id %in% tourneyteams)

# Melt the data for percentile rankings
tourney_team_agg_stats.m <- melt(tourney_team_agg_stats[c("team_name", "team_fgpct", "team_three_fgpct", "team_ptsavg", "team_offreb", "team_rebavg", "team_ast", "team_to", "team_stl", "team_blk", "opp_team_fgpct", "opp_team_three_fgpct", "opp_team_ptsavg", "opp_team_offreb", "opp_team_rebavg", "opp_team_ast", "opp_team_to", "opp_team_stl", "opp_team_blk", "team_avg_ptsdiff", "opp_team_avg_ptsdiff")])
tourney_team_agg_stats.m[is.na(tourney_team_agg_stats.m)] <- 0
tourney_team_agg_stats.rescale <- ddply(tourney_team_agg_stats.m, .(variable), transform, rescale = scale(value))
tourney_team_agg_stats.percentile <-  ddply(tourney_team_agg_stats.m, .(variable), transform, percentile=ecdf(value)(value))

# Create lists of variables we want to plot, by dataframe
ind.player.plot <- c("three_fga", "three_fgm", "ast", "blk", "fga", "fgm", "fouls", "fta", "ft", "pts", "defreb", "offreb", "totreb", "stl", "to")
ind.team.plot <- c("three_fgpct", "three_fga", "three_fgm", "ast", "blk", "fgpct", "fga", "fgm", "fouls", "ftpct", "fta", "ft", "pts", "defreb", "offreb", "stl", "totreb", "to")
agg.team.plot <- c("three_fgpct", "three_fga", "three_fgm", "ast", "forward_avg_height", "guard_avg_height", "avg_ptsdiff", "away_avg_ptsdiff", "home_avg_ptsdiff", "ptsavg", "rebavg", "blk", "fgpct", "fga", "fgm", "fouls", "ftpct", "fta", "ft", "losses", "away_losses", "home_losses", "forward_points", "guard_points", "pts", "defreb", "offreb", "totreb", "stl", "to", "winpct", "away_wins", "home_wins", "wins")
type.plot <- c("Variation","Time","Comparative","Percentile")
colnames(agg.team)[58] <- "team_winpct"

# Mean/Min/Max
range_team_agg_stats <- data.frame(stats=c("min", "mean", "max"))
for (variable in seq(along=agg.team.plot)) {
  gen_var_name <- agg.team.plot[variable]
  command <- paste0("try(range_team_agg_stats$", gen_var_name, " <- c(min(agg.team$team_", gen_var_name, ", na.rm=TRUE), mean(agg.team$team_", gen_var_name, ", na.rm=TRUE), max(agg.team$team_", gen_var_name, ", na.rm=TRUE)))")
  eval(parse(text=command))
}


var_names <- read.csv(file="var.csv")
#data_names <- c("agg.player","agg.team","ind.player","ind.team")
#for(i in 1:length(data_names)){
#  cp_names <- colnames(eval(parse(text=data_names[i])))
#  cp_names <- sapply(seq(1:length(cp_names)),function(y){
#    index <- match(cp_names[y],var_names$Var)
#    if(is.na(index)){
#      val <- cp_names[y]
#    }else{
#      val <- var_names$Names[index]
#    }
#    return(as.character(val))
#  })
#  command <- paste("colnames(",data_names[i],") <- cp_names")
#  eval(parse(text=command)) 
#}

plot_names <- c("ind.player.plot","ind.team.plot","agg.team.plot")
for(i in 1:length(plot_names)){
  pp_names <- eval(parse(text=plot_names[i]))
  pp_names <- sapply(seq(1:length(pp_names)),function(y){
    index <- match(pp_names[y],var_names$Var)
    if(is.na(index)){
      val <- pp_names[y]
    }else{
      val <- var_names$Names[index]
    }
    return(as.character(val))
  })
  command <- paste(plot_names[i],"<- pp_names")
  eval(parse(text=command)) 
}
