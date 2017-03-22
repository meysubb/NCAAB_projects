## Unfinished Optimization 

################# Error Fixing Automation
### Encountered some issues with using apply to fix them. 
#err_df$ind <- sapply(seq(along = err_df$team_name), function(x) {
#  which(agg.team$team_name == err_df$team_name[x])
#})
#err_df$ind <- gsub("\\..*","",rownames(agg.team[err_df$ind,]))


# Create Average
#err_pct <- err_df %>% filter(grepl("pct",err_df$stat))
#err_pct <-  err_pct %>% mutate(var1 = paste0(gsub("opp_|pct","",stat),"m"),
#                               var2 = paste0(gsub("opp_|pct","",stat),"a"))
#err_pct$func <- "createavg"
# General Average
#err_avg <- err_df %>% filter(grepl("avg",err_df$stat))
#err_avg <- err_avg %>% mutate(var1 = gsub("opp_|avg","",stat))
#err_avg$var1 <- gsub("reb","totreb",err_avg$var1)
#err_avg$fun <- "avg"
# Create Sum
#err_sum <- err_df %>% filter(!(grepl("avg",err_df$stat) | grepl("pct",err_df$stat)))
#err_sum <- err_sum %>% mutate(var1 = gsub("opp_","",stat))
#err_sum$fun <- "sum"
#err_avg$ind <- as.numeric(err_avg$ind)
#err_pct$ind <- as.numeric(err_pct$ind)


#apply(err_avg, 1, function(x) recalc_opp_stat(x[4],agg.team,ind.team,x[5],fun=x[6]))
#apply(err_pct,1,function(x) recalc_opp_stat(x[4],agg.team,ind.team,x[5],x[6],fun=x[7])) 

