library(dplyr)
library(ggplot2)
library(reshape)
library(plyr)
library(leaflet)
library(highcharter)
library(stringr)
library(DT)
library(plotly)
# Make sure to load all relevant CSV files

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
## Save agg.team
agg.team <- readRDS(file="data/agg_team.RDS")

agg.team <- round_df(agg.team,2)
## Save agg.player
agg.player<- readRDS(file="data/agg_player.RDS")

## Save ind.team
ind.team <- readRDS(file="data/ind_team.RDS")

## Load ind.player
ind.player <- readRDS(file="data/ind_player.RDS")
### Look up DF
lookup_df <- readRDS(file="data/lookup.RDS")
lookup_df <- lookup_df[-(48:54),]
lookup_df <- lookup_df %>% filter(df_loc != "agg.team")
rownames(lookup_df) <- seq(1:nrow(lookup_df))
team_names <- sort(unique(agg.team$team_name))
# list of variables are imported from script that created them
#,"Percentile"
type.plot <- c("Variation","Time","Compare")

idlookup <- function(values,data,col_to_look){
  ind <- which(data[[col_to_look]] %in% values) 
  id_val <- data$id[ind]
}

ttvars <- c("team_name","wins", "losses","opp_team_name" ,"ptsdiff")
tt <- tooltip_table(
  ttvars,
  sprintf("{point.%s}", ttvars), img = tags$img(src="{point.flagicon}", style = "text-align: center;")
)

source("plot_funcs.R")
