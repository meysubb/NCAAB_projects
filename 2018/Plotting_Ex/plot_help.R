## Plot_Help Function

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
      guides(fill=FALSE)
    return(team_interest)
  }else{
    plot_title <- paste("5 similar teams to",team1)
    team_sim_plot <- ggplot(team_data,aes(x=game_num,y=roll_NetRTG)) + 
      geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
      geom_hline(yintercept =0,linetype='dashed') + 
      scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
      facet_wrap(~team_name,ncol=1) + 
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
      guides(fill=FALSE)
    return(team_sim_plot)
  }
}


### Test of plots - make sure to implement this in shiny dashboard.
#tam_tex <- create_roll_df(ind.team,"Texas A&M","Texas")
#tam_tex_plot <- create_roll_plot(tam_tex)

### similarity plots or source clustering names
#source("clustering_data.R")
#top_teams_names1 <- names(tail(sort(cos_vals[,"Texas A&M"]),5))

#tam_sim <- create_roll_df(ind.team,"Texas A&M",team_lst = top_teams_names1,similarity = T)
#tam_sim_plot <- create_roll_plot(tam_sim,team1="Texas A&M",similarity = T)

