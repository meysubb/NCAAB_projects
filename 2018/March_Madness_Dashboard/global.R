library(dplyr)
library(ggplot2)
library(reshape)
library(highcharter)
library(stringr)
library(DT)
library(plotly)
library(knitr)
library(tidyr)
library(shinycssloaders)


source("plot_help.R")

### Dropbox access
library(rdrop2)
token <- readRDS("my-token.rds")
drop_acc(dtoken = token)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

## revisit the csv cleaning, 
agg.team <- drop_read_csv("NCAAB_data_Shiny/final_data/agg_team.csv")
agg.team <- round_df(agg.team,2)
## Save agg.player
agg.player <- drop_read_csv("NCAAB_data_Shiny/final_data/agg_player.csv")
## Save ind.team
ind.team <- drop_read_csv("NCAAB_data_Shiny/final_data/ind_team.csv")

## Load ind.player
ind.player <- drop_read_csv("NCAAB_data_Shiny/final_data/ind_player.csv")
### Load SOS
sos <- drop_read_csv("NCAAB_data_Shiny/final_data/SOS_dat.csv")

clus_mat <- readRDS("data/cosine_team_matrix_2018.RDS")

conf_team_dat <- drop_read_csv("NCAAB_data_Shiny/final_data/conf_team.csv")

team_names <- sort(unique(agg.team$team_name))
