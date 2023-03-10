library(tidyverse)
library(hoopR)
library(zoo)
library(ggthemes)
library(gt)
library(cfbplotR)


prepare_advanced_conf_metrics <- function(conf_interest,alt_conf_name=NULL){
  mbb_sched <- load_mbb_schedule(seasons = 2023)
  
  # SEC = 23 (conference ID)
  conf_id <- espn_mbb_conferences() %>% filter(short_name == conf_interest) %>% pull(group_id)
  
  mbb_complete <- mbb_sched %>% filter(status_type_completed==TRUE,(home_conference_id==conf_id | away_conference_id==conf_id))
  
  sec_games <- mbb_complete %>% pull(game_id)
  
  mbb_box_score <- load_mbb_team_box(seasons = 2023) 
  
  sec_box_scores <- mbb_box_score %>% filter(game_id %in% sec_games) %>% separate(
    field_goals_made_field_goals_attempted,
    into = c("fgm", "fga"),
    sep = "-"
  ) %>%
    separate(
      free_throws_made_free_throws_attempted,
      into = c("ftm", "fta"),
      sep = "-"
    ) %>%
    separate(
      three_point_field_goals_made_three_point_field_goals_attempted,
      into = c("three_fgm", "three_fga"),
      sep = "-"
    ) %>% 
    mutate(across(fgm:total_turnovers ,as.numeric))
  
  ## four factors 
  
  ### Create Advanced Metrics NETRTG ETC
  
  
  sec_pivoted <- sec_box_scores %>% select(game_id,game_date,team_id,team_abbreviation,team_short_display_name,
                                           fgm:total_turnovers) %>% 
    group_by(game_id) %>% 
    mutate(
      points = 2 * (fgm-three_fgm) + 3 * three_fgm + 1 * ftm,
      # four factors
      team_efg_pct = (fgm + 0.5 * three_fgm) / fga,
      team_ft_rate = fta/fga,
      # pace based
      poss = round(fga + (0.44 * fta) - offensive_rebounds + total_turnovers),
      team_tov_pct = total_turnovers/poss,
      points_per_poss = points/(fga + 0.475 * fta) * (fga + 0.475 * fta)/poss,
      team_type = c("team_a","team_b")
    ) %>% 
    pivot_wider(
      names_from = "team_type",
      values_from = team_id:points_per_poss
    ) %>% 
    mutate(
      ## do all this below for both team a and b
      oreb_pct_team_a = offensive_rebounds_team_a/(defensive_rebounds_team_b+offensive_rebounds_team_a),
      oreb_pct_team_b = offensive_rebounds_team_b/(defensive_rebounds_team_a+offensive_rebounds_team_b),
      ppp_diff_team_a = points_per_poss_team_a - points_per_poss_team_b,
      ppp_diff_team_b = -1 * ppp_diff_team_a,
      ORTG_team_a = 100 * (points_team_a/poss_team_a),
      ORTG_team_b = 100 * (points_team_b/poss_team_b),
      DRTG_team_a = ORTG_team_b,
      DRTG_team_b = ORTG_team_a,
      win_team_a = as.numeric(points_team_a > points_team_b),
      win_team_b = as.numeric(points_team_b > points_team_a)
    )
  
  sec_adv_stats <- sec_pivoted %>% select(game_id,game_date,
                                          team_id_team_a,team_id_team_b,
                                          ORTG_team_a,ORTG_team_b,
                                          DRTG_team_a,DRTG_team_b,
                                          team_efg_pct_team_a,team_efg_pct_team_b,
                                          team_tov_pct_team_a,team_tov_pct_team_b,
                                          team_ft_rate_team_a,team_ft_rate_team_b,
                                          oreb_pct_team_a,oreb_pct_team_b,
                                          win_team_a,win_team_b) %>% 
    mutate(team_id_team_a = as.numeric(team_id_team_a),
           team_id_team_b = as.numeric(team_id_team_b)) %>% 
    pivot_longer(-c(game_id, game_date), names_to = c(".value",NA), names_pattern = c("(.+)_(team_.)")) %>% 
    group_by(team_id) %>% 
    arrange(game_date) %>% 
    mutate(game_num = row_number(),
           NetRTG = ORTG - DRTG,
           roll_NetRTG = rollapply(NetRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
    mutate(team_id = as.character(team_id))
  
  
  if(is.null(alt_conf_name)){
    alt_conf_name <- conf_interest
  }
  conf_team_df <- hoopR::teams_links %>% filter(Conf==alt_conf_name,Year==max(Year))
  team_names <- conf_team_df %>% pull(Team) %>% unique()
  #team_names <- gsub("[.]","",team_names)
  team_names <- cfbplotR::clean_school_names(team_names)
  
  teams <- hoopR::espn_mbb_teams() %>% filter(team %in% team_names) 
  
  
  sec_only_adv <- sec_adv_stats %>% inner_join(teams) %>% 
    mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))
  
  return(sec_only_adv)
}

create_adv_metrics_summary <- function(conf_df,conf_name){
  adv_metrics_sum <- conf_df %>% select(team_id,team,color,logo,team_efg_pct,
                                              team_tov_pct,team_ft_rate,oreb_pct,
                                          ORTG,DRTG) %>% 
    group_by(team_id,team) %>% 
    summarise(across(team_efg_pct:DRTG,mean)) %>% 
    ungroup() 
  
  sched <- conf_df %>% select(team,game_num,win) %>% 
    group_by(team) %>% 
    arrange(game_num,.by_group = T) %>% 
    select(-game_num) %>% 
    dplyr::summarize(wins=list(win), .groups = "drop")
  
  final <- adv_metrics_sum %>% left_join(sched)
  
  title <- glue::glue("{conf_name} Advanced Stats Review")
  
  final %>% 
    select(-team_id) %>% 
    mutate(
      NetRTG = ORTG - DRTG
    ) %>% 
    arrange(-NetRTG) %>% 
    select(team,NetRTG,ORTG,DRTG,everything()) %>% 
    gt() %>% 
    gt_fmt_cfb_logo(columns = "team") %>% 
    fmt_number(
      columns = contains("RTG"),
      decimals = 2
    ) %>% 
    fmt_percent(
      columns = team_efg_pct:oreb_pct,
      decimals = 2
    ) %>% 
    gtExtras::gt_color_rows(
      NetRTG:DRTG,
      palette = "ggsci::blue_material"
    ) %>% 
    data_color(
      columns = team_efg_pct:oreb_pct,
      colors = color_fn
    ) %>% 
    gtExtras::gt_plt_winloss(wins,
                             max_wins = 31) %>% 
    cols_label(
      team = "",
      team_efg_pct = "eFG %",
      team_ft_rate = "FT Rate",
      oreb_pct = "OREB %",
      team_tov_pct = "TOV %",
      wins = "Schedule"
    ) %>% 
    tab_header(
      title = md(title),
      subtitle = md("NetRTG (Per 100 possessions)")
    ) %>% 
    tab_footnote(
      footnote = md("@msubbaiah1")
    )
}


color_fn <- scales::col_numeric(
  domain = NULL,
  palette = as.character(
    paletteer::paletteer_d(
      palette = "ggsci::red_material" ,
      type = "continuous"))
)


create_roll_plot <- function(team_data,team1=NULL,similarity=FALSE){
    team_interest <- ggplot(team_data,aes(x=game_num,y=roll_NetRTG)) + 
      geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
      geom_hline(yintercept =0,linetype='dashed') + 
      scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
      facet_wrap(~team) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) + 
      labs(x="Game",y='NetRTG',
           title="Net Rating in 3-Game Rolling Intervals",
           subtitle = 'NetRTG (Per 100 possessions)',
           caption="@msubbaiah1") + 
      theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
            axis.title.x = element_text(size=16),
            plot.subtitle = element_text(size=12),
            plot.caption = element_text(color="#999999")) + 
      theme(panel.spacing = unit(2, "lines")) + 
      guides(fill=FALSE)
    return(team_interest)
}


conf_interest <- "SEC"
sec_df <- prepare_advanced_conf_metrics("SEC")
create_adv_metrics_summary(sec_df,"SEC") %>% gtsave("sec.png")
create_roll_plot(sec_df)

conf_interest <- "Big 12"
big12_df <- prepare_advanced_conf_metrics("Big 12","B12") 
create_adv_metrics_summary(big12_df,"Big 12") %>% gtsave("big_12.png")
create_roll_plot(big12_df)

big10_df <- prepare_advanced_conf_metrics("Big Ten","B10")
create_adv_metrics_summary(big10_df,"Big 10") %>% gtsave("big_10.png")
create_roll_plot(big10_df)


big10_df <- prepare_advanced_conf_metrics("ACC","ACC")
create_adv_metrics_summary(big10_df,"ACC") %>% gtsave("acc.png")
create_roll_plot(big10_df)
