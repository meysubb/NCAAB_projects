## Plot_Help Function
### Team Rolling NRTG functions
create_roll_df <- function(team_data,team1,team2=NULL,team_lst=NULL,similarity=FALSE){
  require(zoo)
  if(!similarity){
    team <- team_data %>% 
      select(team_name,opp_team_name,ORTG,DRTG,game_date,wins,losses,ptsdiff)  %>% 
      filter(team_name %in% c(team1,team2)) %>% 
      group_by(team_name) %>% 
      arrange(game_date) %>% 
      mutate(game_num = row_number(),
             NRTG = ORTG - DRTG,
             streakW = cumsum(ifelse(ptsdiff>0,1,0)),
             streakL =cumsum(ifelse(ptsdiff<0,1,0)),
             roll_NetRTG = rollapply(NRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
      mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))    
  }
  else{
    team <- ind.team %>% 
      select(team_name,opp_team_name,ORTG,DRTG,game_date,wins,losses,ptsdiff) %>%
      filter(team_name %in% team_lst) %>% 
      group_by(team_name) %>% 
      arrange(game_date) %>% 
      mutate(game_num = row_number(),
             NRTG = ORTG - DRTG,
             streakW = cumsum(ifelse(ptsdiff>0,1,0)),
             streakL =cumsum(ifelse(ptsdiff<0,1,0)),
             roll_NetRTG = rollapply(NRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
      mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))
  }
  return(team)
}

create_roll_plot <- function(team_data,team1=NULL,similarity=FALSE){
  require(ggthemes)
  if(!similarity){
    team_interest <- ggplot(team_data,aes(x=game_num,y=roll_NetRTG)) + 
      geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
      geom_hline(yintercept =0,linetype='dashed') + 
      scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
      facet_wrap(~team_name) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) + 
      labs(x="Game",y='',
           title="Net Rating in 3-Game Rolling Intervals",
           subtitle = 'NetRTG (Per 100 possessions)',
           caption="@msubbaiah1") + 
      theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
            axis.title.x = element_text(size=16),
            plot.subtitle = element_text(size=12),
            plot.caption = element_text(color="#999999")) + 
      theme(panel.spacing = unit(3, "lines")) + 
      guides(fill=FALSE)
    return(team_interest)
  }else{
    plot_title <- paste("5 similar teams to",team1)
    team_sim_plot <- ggplot(team_data,aes(x=game_num,y=roll_NetRTG)) + 
      geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
      geom_hline(yintercept =0,linetype='dashed') + 
      scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
      facet_wrap(~team_name,nrow=1) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) + 
      labs(x="Game",y='',
           title=plot_title,
           subtitle = 'Rolling 3 game - NetRTG (Per 100 possessions)',
           caption="@msubbaiah1") + 
      theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
            axis.title.x = element_text(size=16),
            plot.subtitle = element_text(size=12),
            plot.caption = element_text(color="#999999")) + 
      theme(panel.spacing = unit(2, "lines")) + 
      guides(fill=FALSE)
    return(team_sim_plot)
  }
}


#### Morey Index Plot
morey_index <- function(team_dat,t_name,t_name2=NULL,team_lst1=NULL,similarity=F,col=NULL){
  plot_df <- team_dat %>% 
    mutate(team_M_Index = (team_fta + team_three_fga)/team_fga,
           opp_M_Index = (opp_team_fta + opp_team_three_fga)/opp_team_fga,
           poss_per_game = team_possessions/ (team_wins + team_losses))
  if(similarity){
    plot_df_sim <- plot_df %>% filter(team_name %in% team_lst1)
    plot_final <- ggplot(plot_df,aes(x=poss_per_game,y=team_M_Index)) + 
      geom_point(color="grey",alpha=0.7) + 
      geom_point(data=plot_df_sim,aes(x=poss_per_game,y=team_M_Index),color=col,size=3) + 
      theme_fivethirtyeight(base_size=16) + 
      theme(axis.title = element_text()) +
      labs(x='Possessions Per Game',
           y="Morey Index \n(FTA + 3FGA)/FGA",
           title="Morey Index vs. Pace",
           subtitle=paste0("Similarity Comparision - ",t_name),
           caption="@msubbaiah1") 
  }else{
    plot_df_t1 <- plot_df %>% filter(team_name==t_name)
    plot_df_t2 <- plot_df %>% filter(team_name==t_name2)
    plot_final <- ggplot(plot_df,aes(x=poss_per_game,y=team_M_Index)) + 
      geom_point(color="grey",alpha=0.7) + 
      geom_point(data=plot_df_t1,aes(x=poss_per_game,y=team_M_Index),color="red",size=3) + 
      geom_point(data=plot_df_t2,aes(x=poss_per_game,y=team_M_Index),color="blue",size=3) + 
      theme_fivethirtyeight(base_size=16) + 
      theme(axis.title = element_text()) +
      labs(x='Possessions Per Game',
           y="Morey Index \n(FTA + 3FGA)/FGA",
           title="Morey Index vs. Pace",
           caption="@msubbaiah1") 
  }
  return(plot_final)
}

### Dean Oliver Four Factors

four_factors_df <- function(team_df,t_name,t_name2=NULL,team_lst1=NULL,similarity=F){
  if(similarity){
    ts <- c(t_name,team_lst1)
    t_test <- team_df %>% filter(team_name %in% ts) %>% 
      mutate(team_tov_pct = team_to/(team_fga + 0.44 * team_fta + team_to),
             team_ft_rate = team_ft/team_fga,
             team_oreb_pct = team_offreb/(opp_team_defreb+team_offreb)) %>% 
      select(team_name,team_efg_pct,team_tov_pct,team_ft_rate,
             team_oreb_pct)
    do_factors <- t_test %>% 
      gather(variable, value, -team_name) 
    do_factors$variable <- as.factor(do_factors$variable)
    levels(do_factors$variable) <- c("eFG %","FT Rate","OREB %","TOV %")
  }else{
    t_test <- team_df %>% filter(team_name %in% c(t_name,t_name2))
    conf_name <- unique(t_test$Conf)
    conf_do_fac <- team_df %>% filter(Conf %in% conf_name) %>% 
      mutate(team_tov_pct = team_to/(team_fga + 0.44 * team_fta + team_to),
             team_ft_rate = team_ft/team_fga,
             team_oreb_pct = team_offreb/(opp_team_defreb+team_offreb)) %>% 
      select(team_name,team_efg_pct,team_tov_pct,team_ft_rate,
             team_oreb_pct,Conf)
    
    do_factors <- conf_do_fac %>% 
      gather(variable, value, -team_name,-Conf) 
    do_factors$variable <- as.factor(do_factors$variable)
    levels(do_factors$variable) <- c("eFG %","FT Rate","OREB %","TOV %")
  }
  return(do_factors)
}

four_factors_plot <- function(fac_df,similarity=F){
  if(similarity){
    p <- ggplot(fac_df, aes(team_name, variable, fill = value)) + 
      geom_tile(colour = "white") + 
      geom_text(aes(x = team_name, y = variable, label = sprintf("%1.2f%%", round(value*100, digits = 2))),color='white') + 
      scale_fill_gradient(low = "blue", high = "firebrick3") + 
      labs(x="",y="",title="Dean Oliver's Four Factors",
           caption='@msubbaiah1') + 
      theme_fivethirtyeight(base_size=16) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      guides(fill=FALSE)
  }else{
    p <- ggplot(fac_df, aes(team_name, variable, fill = value)) + 
      geom_tile(colour = "white") + 
      geom_text(aes(x = team_name, y = variable, label = sprintf("%1.2f%%", round(value*100, digits = 2))),color='white') + 
      facet_wrap(~Conf,scales="free_y") + 
      scale_fill_gradient(low = "blue", high = "firebrick3") + 
      coord_flip() + 
      labs(x="",y="",title="Dean Oliver's Four Factors",
           caption='@msubbaiah1') + 
      theme_fivethirtyeight(base_size=16) +
      guides(fill=FALSE)
  }
  return(p)
}


### Create Player Plot 
### USG vs. TS%
create_usg_df <- function(player_data,team1){
    conf_team_dat_partial <- conf_team_dat %>% select(-team_name)
    team_dat_2 <- inner_join(player_data,conf_team_dat_partial,by="TeamID") 
    
    team_dt <- team_dat_2 %>% filter(team_name==team1,minutes>50,fga>25)
    conf_name <- unique(team_dt$Conf)
    conf_df <- team_dat_2 %>% filter(Conf==conf_name,minutes>50,fga>25)
    list_df <- list(team_dt,conf_df)
    return(list_df)
}
  
create_usg_plot <- function(lst_usg,team1,col){
  ## break the list out
  tit <- paste0("Player Impact - ",team1)
  dat_usg <- lst_usg[[2]]
  team_usg <- lst_usg[[1]]
  
  x <- c("Player Name: ","Team Name: ", "USG Rate: ","TS %: ")
  y <-c ("{point.player_name}","{point.team_name}", 
         sprintf("{point.%s:.3f}", c("usg_rate", "ts_pct")))
  tltip <- tooltip_table(x,y)
  
  p <- highchart() %>%
    hc_add_series(data=dat_usg,hcaes(x=usg_rate,y=ts_pct),type='scatter',color=hex_to_rgba("#707070", alpha = 0.4),marker = list(symbol = "circle")) %>% 
    hc_add_series(data=team_usg,hcaes(x=usg_rate,y=ts_pct),type='scatter',color=col,marker = list(symbol = "circle",size=2)) %>% 
    hc_legend() %>% 
    hc_yAxis(title = list(text = "TS %")) %>%
    hc_xAxis(title=list(text="Usage Rate")) %>% 
    hc_title(text = tit,align = "left") %>% 
    hc_subtitle(text="Compared to respective conference players",align="left") %>% 
    hc_credits(
      enabled = TRUE,
      text = "@msubbaiah1",
      href = "https://twitter.com/msubbaiah1"
    ) %>% hc_add_theme(hc_theme_flat()) %>% 
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
  return(p)
}
### Create Player DF and PLOT
### % Mins vs % Shots
player_min_df <- function(player_df,conf_df,team1){
  conf_df <- conf_df %>% select(-team_name)
  agg_conf <- inner_join(player_df,conf_df,by="TeamID")
  team1 <- agg_conf %>% filter(team_name==team1,
                               minutes>50,fga>25) %>% mutate(
    pct_mins = minutes/sum(minutes))
  
  conf_name <- unique(team1$Conf)
  
  conf_teams <- agg_conf %>% filter(Conf==conf_name,minutes>50) %>% 
    group_by(team_name) %>% mutate(
      pct_mins = (minutes/sum(minutes))*100,
      pct_shots = (fga/sum(fga))*100,
      pct_3pts = (three_fga/sum(three_fga))*100)
  return(conf_teams)
}

player_min_plot <- function(plot_df,team1){
  label_names <- plot_df %>% filter(pct_shots>quantile(plot_df$pct_shots,probs=seq(0,1,0.05))[19])
  
  x <- c("Player Name: ","Team Name: ", "% Mins: ","% Shots: ","% 3PTers: ")
  y <-c ("{point.player_name}","{point.team_name}", 
         sprintf("{point.%s:.3f}", c("pct_mins", "pct_shots","pct_3pts")))
  tltip <- tooltip_table(x,y)
  
  n_val <- length(unique(plot_df$team_name))
  final_n <- round(n_val)/2
  final_n2 <- n_val - final_n
  
  mycolors = c(RColorBrewer::brewer.pal(name="Dark2", n = final_n), 
               RColorBrewer::brewer.pal(name="Paired", n = final_n2))
  
  
  p <- highchart() %>%
    hc_add_series(data=plot_df,hcaes(x=pct_mins,y=pct_shots,size=pct_3pts*2,group=team_name),
                  type='scatter',marker = list(symbol = "circle")) %>% 
    hc_colors(mycolors) %>% 
    hc_legend(layout = "vertical", verticalAlign = "top",
              align = "right", valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "% of Shots"),
             labels = list(format = "{value}%")) %>%
    hc_xAxis(title=list(text="% of Minutes"),
             labels = list(format = "{value}%")) %>% 
    hc_title(text = "Offensive Density",align = "left") %>% 
    hc_subtitle(text="Larger bubbles indicate a 3pt-heavy shooter",align="left") %>% 
    hc_credits(
      enabled = TRUE,
      text = "@msubbaiah1",
      href = "https://twitter.com/msubbaiah1"
    ) %>% hc_add_theme(hc_theme_flat()) %>% 
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
  
  return(p)
}
### Test of plots - make sure to implement this in shiny dashboard.
#tam_tex <- create_roll_df(ind.team,"Texas A&M","Texas")
#tam_tex_plot <- create_roll_plot(tam_tex)

### similarity plots or source clustering names
#source("clustering_data.R")
#top_teams_names1 <- names(tail(sort(cos_vals[,"Texas A&M"]),5))

#tam_sim <- create_roll_df(ind.team,"Texas A&M",team_lst = top_teams_names1,similarity = T)
#tam_sim_plot <- create_roll_plot(tam_sim,team1="Texas A&M",similarity = T)

