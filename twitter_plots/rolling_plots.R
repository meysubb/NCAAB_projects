library(tidyverse)
#library(ncaahoopR)
library(bigballR)

sched_func <- function(x){
  print(x)
  return(get_team_schedule(team.id=x))
}  


sec = teamids %>% filter(Conference == 'SEC',Season=='2019-20') %>% 
  mutate(
    sched = purrr::map(ID,safely(sched_func))
  ) %>% unnest()
 
sec2 = sec %>% unnest(sched) %>% drop_na(Home_Score)  %>% 
  mutate(bs = purrr::map(Game_ID,safely(scrape_box_score))
      )

sec3 = sec2 %>% unnest(bs) %>% unnest(bs)

unique_teams = unique(sec3$Team)

test = lapply(unique_teams,function(idx){
  df <- sec3 %>% filter(Team==idx) %>% mutate(
    team=trimws(team)
  )
  
  team_stats <- df %>% filter(team==idx,Player=="Totals") %>% 
    select(-Player,game_id) %>% distinct(game_id,.keep_all = TRUE)
  colnames(team_stats) <- paste0("team_",colnames(team_stats))
  opp_stats <- df %>% filter(team==idx,Player=="opp_totals") %>% group_by(game_id) %>% 
    select(-Player,game_id) %>% distinct(game_id,.keep_all = TRUE)
  colnames(opp_stats) <- paste0("opp_",colnames(opp_stats))
  
  team_stats <- bind_cols(team_stats,opp_stats)
  #team_stats$team <- idx
  team_pace_data <- team_stats %>% mutate(
    possessions = round(team_FGA + (0.475 * team_FTA) - team_ORebs + team_TO),
    opp_possessions = round(opp_FGA + (0.475 * opp_FTA) - opp_ORebs + opp_TO),
    points_per_poss = team_PTS/(team_FGA + 0.475 * team_FTA) * (team_FGA + 0.475 * team_FTA)/possessions,
    d_points_per_poss = opp_PTS/(opp_FGA + 0.475 * opp_FTA) * (opp_FGA + 0.475 * opp_FTA)/opp_possessions,
    ppp_diff = points_per_poss - d_points_per_poss,
    ORTG = 100 * (team_PTS/possessions),
    DRTG = 100 * (opp_PTS/opp_possessions)
  )
})


all_teams_adv <- bind_rows(test)

all_teams_adv2 <-
  all_teams_adv %>% select(
    team_Team,
    team_Date,
    team_game_id,
    possessions,
    points_per_poss,
    d_points_per_poss,
    ppp_diff,
    ORTG,
    DRTG
  )  %>% group_by(team_Team) %>% mutate(team_Date = as.Date(team_Date,format="%m/%d/%Y")) %>% 
  arrange(team_Date) %>%
  mutate(game_num = row_number(),
         NetRTG = ORTG - DRTG,
         roll_NetRTG = rollapply(NetRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup()


league_avg <- all_teams_adv2 %>% group_by(game_num) %>% summarise_all(funs(mean)) %>% ungroup() %>% 
  mutate(lg_roll_NetRTG = rollapply(NetRTG,3,FUN=mean,align='right',fill=NA)) %>% 
  select(game_num,lg_roll_NetRTG)


library(ggthemes)

ann_text <- data.frame(game_num = 6,roll_NetRTG=-15,lab = "Non-conference",
                       team = factor('Alabama',levels=unique_teams))

ann_text2 <- data.frame(game_num = 20,roll_NetRTG=-15,lab = "Conference",
                        team = factor('Alabama',levels=unique_teams))

final_netrtg_df <- all_teams_adv2 %>%
  group_by(team_Team) %>% 
  filter(row_number()==n()) %>% mutate(
    labe = round(roll_NetRTG,2)
  )


ggplot(all_teams_adv2,aes(x=game_num,y=roll_NetRTG)) + 
  geom_line(color="maroon") + 
  facet_wrap(~team_Team) + 
  geom_line(data=league_avg,aes(x=game_num,y=lg_roll_NetRTG),color="green") + 
  geom_vline(aes(xintercept=13)) + 
  geom_text(data=ann_text,label='Non-conference') + 
  geom_text(data=ann_text2,label='Conference') + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(x="Game",y='',
       title="Net Rating in 3-Game Rolling Intervals",
       subtitle = 'NetRTG (Per 100 possessions); Green line is SEC average',
       caption="@msubbaiah1") + 
  theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
        axis.title.x = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(color="#999999")) + 
  geom_text(data=final_netrtg_df, 
            aes(x=game_num+4,y=roll_NetRTG,label=labe), inherit.aes=FALSE) + 
  xlim(c(0,30))

ggsave("sec_ntrg.jpg",height=9/1.2,width=16/1.2)
