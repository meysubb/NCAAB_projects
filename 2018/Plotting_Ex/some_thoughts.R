## work stuffs

agg.team <- read_csv("final_data/agg_team.csv")
### calculate morey Index for team and opponnet
tamu <- agg.team %>% 
  mutate(team_M_Index = (team_fta + team_three_fga)/team_fga,
         opp_M_Index = (opp_team_fta + opp_team_three_fga)/opp_team_fga,
         poss_per_game = team_possessions/ (team_wins + team_losses))
### plot Morey Index
ggplot(tamu,aes(x=poss_per_game,y=team_M_Index)) + 
  geom_point() + 
  facet_wrap(~Conf) + theme_bw(base_size=16) + 
  labs(x='Possessions Per Game',
       y="Morey Index \n(FTA + 3FGA)/FGA",
       title="Morey Index vs. Pace",
       caption="@msubbaiah1") 

b12 <- tamu %>% filter(Conf=="Big 12")


library(ggrepel)
ggplot(b12,aes(x=poss_per_game,y=team_M_Index,label=team_name)) + 
  geom_point() + 
  geom_label_repel() + 
  theme_bw(base_size=16) + 
  labs(x='Possessions Per Game',
       y="Morey Index \n(FTA + 3FGA)/FGA",
       title="Morey Index vs. Pace",
       caption="@msubbaiah1")

ind.team <- read_csv("final_data/ind_team.csv")





t <- ggplot(tamu,aes(x=poss_per_game,y=team_M_Index)) + geom_point() + theme_bw()


#### Player plots
### USG vs. TS
library(ggrepel)
agg.player <- read.csv("final_data/agg_player.csv")

conf_team_dat <- conf_team_dat %>% select(-team_name)
agg_conf <- inner_join(agg.player,conf_team_dat,by="TeamID") 

conf_team_dt <- agg_conf %>% filter(team_name=="Texas A&M",minutes>50,fga>25)
conf_name <- unique(conf_team_dt$Conf)

tamu <- agg_conf %>% filter(Conf==conf_name,minutes>50,fga>25)

test <- ggplot(tamu) + 
  geom_point(aes(x=usg_rate,y=ts_pct),color='grey',alpha=0.4) + 
  geom_point(data=conf_team_dt,aes(x=usg_rate,y=ts_pct),color='firebrick3',size=3) + 
  geom_label_repel(data=conf_team_dt,aes(x=usg_rate,y=ts_pct,label=player_name)) + 
  theme_bw(base_size=16) + 
  labs(x="Usage Rate",y="TS %",title="Player Impact",
       subtitle="Compared against other players in respective conference",
       caption="@msubbaiah1")


### Shots vs Minutes played per team
tamu <- agg_conf %>% filter(team_name=="Texas A&M")

tamu <- tamu %>% mutate(
  pct_mins = minutes/sum(minutes)
)

conf_name <- unique(tamu$Conf)
conf_name <- "Big 12"

conf_teams <- agg_conf %>% filter(Conf==conf_name,minutes>50) %>% 
  group_by(team_name) %>% mutate(
    pct_mins = minutes/sum(minutes),
    pct_shots = fga/sum(fga),
    pct_3pts = three_fga/sum(three_fga)
    ) 

label_names <- conf_teams %>% filter(pct_shots>quantile(conf_teams$pct_shots,probs=seq(0,1,0.05))[19])

library(scales)
ggplot(conf_teams) + 
  geom_point(aes(x=pct_mins,pct_shots,size=pct_3pts,color=team_name)) + 
  geom_text_repel(data=label_names,
                  aes(x=pct_mins,pct_shots,color=team_name,label=player_name),size = 3,
                  fontface = 'bold',segment.color = 'grey50'
  ) +
  facet_wrap(~team_name) + theme_fivethirtyeight(base_size=16) + + 
  guides(color=FALSE,size=FALSE) + 
  labs(x="% of Minutes",
       y="% of Shots",
       title="Offensive Density",
       subtitle="Larger bubbles indicate a 3pt-heavy shooter")  + 
  scale_y_continuous(labels=percent) + 
  scale_x_continuous(labels=percent)
  

#### Dean Oliver's Four Factors
t_test <- agg.team %>% filter(team_name %in% c("Texas A&M","Texas"))
conf_name <- unique(t_test$Conf)

conf_do_fac <- agg.team %>% filter(Conf %in% conf_name) %>% 
  mutate(team_tov_pct = team_to/(team_fga + 0.44 * team_fta + team_to),
         team_ft_rate = team_ft/team_fga,
         team_oreb_pct = team_offreb/(opp_team_defreb+team_offreb)) %>% 
  select(team_name,team_efg_pct,team_tov_pct,team_ft_rate,
         team_oreb_pct,Conf)


do_factors <- conf_do_fac %>% 
  gather(variable, value, -team_name,-Conf) 

do_factors$variable <- as.factor(do_factors$variable)
levels(do_factors$variable) <- c("eFG %","FT Rate","OREB %","TOV %")

ggplot(do_factors, aes(team_name, variable, fill = value)) + 
  geom_tile(colour = "white") + 
  geom_text(aes(x = team_name, y = variable, label = sprintf("%1.2f%%", round(value*100, digits = 2))),color='white') + 
  facet_wrap(~Conf,scales="free_y") + 
  scale_fill_gradient(low = "blue", high = "firebrick3") + 
  coord_flip() + 
  labs(x="",y="",title="Dean Oliver's Four Factors",
       caption='@msubbaiah1') + 
  theme_fivethirtyeight(base_size=16) +
  guides(fill=FALSE)


### Clustering Players 
### IGNORE ALL OF THIS
if(FALSE){
  agg.pr <- agg_conf %>% filter(minutes>50) %>% select(efg_pct,three_par,reb_rate,ast_rate,blk_rate,usg_rate,
                                                       ptsavg,TeamID) %>% na.omit()
  agg.pr <- agg.pr[!is.infinite(rowSums(agg.pr)),]
  agg.pr2 <- agg.pr %>% select(-TeamID)
  pca_mod <- princomp(agg.pr2,cor = T,scores=T)  # compute principal components
  agg.final <- cbind(agg.pr,pca_mod$scores) 
  agg.final <- agg_conf %>% select(player_name,team_name,TeamID,efg_pct,three_par,
                                   reb_rate,ast_rate,blk_rate,usg_rate,
                                   ptsavg,Conf) %>% inner_join(.,agg.final)
  sec <- agg.final %>% filter(Conf=='SEC')
  ggplot(sec, aes(Comp.1, Comp.2)) + 
    geom_point() +  
    theme_bw() + 
    guides(col=FALSE)
}
