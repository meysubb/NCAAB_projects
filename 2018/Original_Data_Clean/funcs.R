### functions to clean NCAA data. 
time_to_mins <- function(df, col_name) {
  x <- strsplit(as.character(df[, col_name]), ":")
  sapply(x, function(x) {
    as.numeric(x[1]) + as.numeric(x[2]) / 60
  })
}

# 12260 is year id
create_team_id <- function(df, year_id) {
  gsub(year_id, "", gsub("\\D", "", df))
}

# Order stats to see errors.
find_errors <- function(agg_df, statistic, nhead = 6) {
  head(agg_df[rev(order(agg_df[, statistic])), c("team_name", statistic)], nhead)
}

reshape_team_lvl_data <- function(df,cols_i,new_col_name,prefix){
  sel_cols <- paste0(prefix,cols_i)
  sel_cols <- sel_cols[!(grepl("avg",sel_cols) | grepl("dbl",sel_cols))]
  sel_cols <- c(sel_cols,"game_id")
  
  col_names <- gsub(prefix,"",sel_cols)
  col_names[1:18] <- paste0(new_col_name,col_names[1:18])
  df %>% select_(.dots = sel_cols) %>% rename_(.dots=setNames(names(.),col_names))
}

recalc_opp_stat <-
  function(t_id,
           agg_df,
           ind_df,
           var1,
           var2 = NULL,
           fun = c("sum", "avg", "createavg")) {
    require(dplyr)
    fun <- match.arg(fun)
    t_id  <- paste("/team/", t_id, "/12260", sep = "")
    game_subset <- ind_df[ind_df$team_id %in% t_id,"game_id"]
    if (fun == "createavg") {
      opp_game_subset <-
        ind_df %>% filter(game_id %in% game_subset &
                              !(team_id %in% t_id)) %>% select(one_of(var1), one_of(var2))
    } else {
      opp_game_subset <-
        ind_df %>% filter(game_id %in% game_subset &
                            !(team_id %in% t_id)) %>% select(one_of(var1))
    }
    if (fun == "sum") {
      ret <- sum(opp_game_subset[,1])
    } else if (fun == "avg") {
      ret <- mean(opp_game_subset[,1])
    } else if (fun == "createavg") {
      x <- sum(opp_game_subset[, 1])
      y <- sum(opp_game_subset[, 2])
      if (x <= y) {
        ret <- mean(x / y)
      } else {
        ret <- mean(y / x)
      }
    }
    return(ret)
  }