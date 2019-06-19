#rm(list=setdiff(ls(), "summedStats"))
#saveRDS(summedStats,"blf_stats.RDS")
summedStats <- readRDS("blf_stats.RDS")

df <- summedStats %>% dplyr::select(CurrentTeam,Year,TeamAdjEM2,TeamAdjEM,NewCoach,
                                    Rank_Recruits,Rating_Recruits,RookieWS,
                                    ReturningWS,LastDepartingFIC,
                                    LastTeamAdjEM,Last5TeamAdjEM2,Last3SOS,predWS,predWS3,predUSG3)

for(cutoff in 2009:2019){
  # cutoff<-2015
  fit<-lm(TeamAdjEM2~LastDepartingFIC++LastTeamAdjEM+
            Last5TeamAdjEM2+Last3SOS+predWS3,
          data=df[df$Year%in% 2006:(cutoff-1)& df$predUSG3>0,])
  df$predTeamRk[df$Year==cutoff]<-predict(fit, newdata=df[df$Year==cutoff, ])
  t <- predict(fit, newdata=df[df$Year==cutoff, ],se.fit=T)
  df[df$Year==cutoff,"se.val"] <- t$se.fit
}


library(tidyverse)
ranks_19 <- df %>% dplyr::select(-Last5TeamAdjEM2) %>% dplyr::filter(Year>=2019) %>% dplyr::select(CurrentTeam,predTeamRk,se.val) %>%
  mutate(CurrentTeam = tolower(CurrentTeam))
cost_df <- read_csv("mid_major_cost.csv")
cost_df$Team <- gsub("St.","State",cost_df$Team)
cost_df$Team <- tolower(cost_df$Team)

ranks_19[ranks_19$CurrentTeam == 'brigham young',"CurrentTeam"] <- "byu"
ranks_19[ranks_19$CurrentTeam == 'college of charleston',"CurrentTeam"] <- "charleston"
ranks_19[ranks_19$CurrentTeam == "mount st marys","CurrentTeam"] <- "mount state mary's"


combine_df <- cost_df %>% left_join(ranks_19,by=c("Team"="CurrentTeam"))

library(ggplot2)
ggplot(combine_df,aes(x=Cost,y=predTeamRk)) + 
  geom_point() + 
  geom_smooth() + 
  theme_bw() + 
  labs(x="Mid-Major Team Cost",y="Predicted Rank from Algorithm",
       subtitle="@msubbaiah1",
       title = "Something here")


set.seed(41)
combine_df <- combine_df %>% rowwise() %>% 
mutate(
  ret = Cost * (1+rnorm(1,mean=0,sd=se.val)),
  ret2 = ret * (1+rnorm(1,mean=0,sd=se.val)),
  ret3 = ret2 * (1+rnorm(1,mean=0,sd=se.val)),
  ret4 = ret3 * (1+rnorm(1,mean=0,sd=se.val)),
  ret5 = ret4 * (1+rnorm(1,mean=0,sd=se.val)),
  ret6 = ret5 * (1+rnorm(1,mean=0,sd=se.val)),
  ret7 = ret6 * (1+rnorm(1,mean=0,sd=se.val)),
  ret8 = ret7 * (1+rnorm(1,mean=0,sd=se.val)),
  ret9 = ret8 * (1+rnorm(1,mean=0,sd=se.val))
)


### returns matrix
price_mat <- combine_df %>% dplyr::select(Team,Cost,starts_with('ret'))
price_mat <- as.matrix(t(price_mat))
colnames(price_mat) <- price_mat[1,]
price_mat <- price_mat[-1,]
price_mat <- apply(price_mat,2,as.numeric)
ret_mat <- diff(price_mat)

ret_mat2 <- ret_mat/price_mat[-nrow(price_mat),]
ret_mat2 <- apply(ret_mat,2,median)

### start
library(lpSolveAPI)
n = length(ret_mat2)
fund_allocate <- make.lp(0,n)
yNames <- names(ret_mat2)
colnames(fund_allocate) <- c(yNames)

costs <- combine_df %>% pull(Cost)
set.objfn(fund_allocate,c(matrix(ret_mat2, ncol=1, byrow=TRUE)))
add.constraint(fund_allocate,costs,"<=",500)
for(i in 1:n){
  add.constraint(fund_allocate,1,"<=",10,c(i))
}
#add.constraint(fund_allocate,rep(1,n),"<=",15)
lp.control(fund_allocate,sense='max')
set.type(fund_allocate, 1:ncol(fund_allocate), type = 'integer')
write.lp(fund_allocate,'funds.lp',type = 'lp')

status = solve(fund_allocate) 
print(get.objective(fund_allocate)) 
solution = get.variables(fund_allocate)

yNames[which(solution!=0)]
solution[which(solution!=0)]
costs[which(solution!=0)]
sum(solution[which(solution!=0)] * costs[which(solution!=0)])
write.table(costs[which(solution!=0)],"example.xls")


