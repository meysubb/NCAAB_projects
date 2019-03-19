library(tidyverse)
cos_mat <- readRDS("../March_Madness_Dashboard/data/cosine_team_matrix_2018.RDS")

conf <- read_csv("~/Dropbox/NCAAB_data_Shiny/final_data/conf_team.csv") %>% select(-X1)

dissimilarity = 1-cos_mat
amax = max(dissimilarity)
adist = dissimilarity / amax

mds <- cmdscale(adist)

mds <- as.data.frame(mds) %>% rownames_to_column()
colnames(mds) <- c("team","x","y")

mds_v <- mds %>% mutate(team = trimws(tolower(team))) %>% 
  inner_join(.,conf,by=c("team"="team_name"))

library(ggrepel)

ggplot(mds_v,aes(x=x,y=y)) + 
  geom_point() + theme_bw() +
  facet_wrap(~Conf)
