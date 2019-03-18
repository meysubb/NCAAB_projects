library(tabulizer)
### https://extra.ncaa.org/solutions/rpi/SitePages/Home.aspx <- to get pdf
location <- "raw_data/funcs.pdf"
out <- extract_tables(location,encoding="UTF-8")
### 

raw_dat <- bind_rows(lapply(out,function(x){
  as.data.frame(x[-1,])}))

raw_dat <- raw_dat[-233,]

raw_dat2 <- raw_dat %>% mutate(
  V1 = gsub("\\[|\\]","",V1)
) %>% separate(V11,c("Q1-W","Q1-L","Q2-W","Q2-L","Q3-W","Q3-L","Q4-W","Q4-L"))


colnames(raw_dat2)[1:10] <- c("team_name","NET","avg_opp_net","avg_opp_rank","record",
                        "conf_record","non_conf_record","road_record","SOS",
                        "NC_SOS")

sos_clean <- raw_dat2 %>% mutate(
  record_wins = gsub("-.*","",record),
  record_losses = gsub(".*-","",record),
  non_conf_record_wins = gsub("-.*","",non_conf_record),
  non_conf_record_losses = gsub(".*-","",non_conf_record),
  conf_record_wins = gsub("-.*","",conf_record),
  conf_record_losses = gsub(".*-","",conf_record),
  road_record_wins = gsub("-.*","",road_record),
  road_record_losses = gsub(".*-","",road_record)
)



write.csv(sos_clean,file="final_data_new/SOS_dat.csv",row.names = F)
 