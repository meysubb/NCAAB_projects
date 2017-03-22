library(highcharter)

tamu <- ind.team %>% select(team_name,opp_team_name,ptsdiff,game_date,wins,losses) %>% filter(team_name == "Texas A&M")
tamu <- dplyr::arrange(tamu, game_date)
tamu$wins <- cumsum(tamu$ptsdiff>0)
tamu$losses <- cumsum(tamu$ptsdiff<0)

hc <- hchart(tamu,"column", hcaes(game_date, ptsdiff)) %>% 
  hc_title(text = "Texas A&M Season") %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(title = list(text = "Game Date")) %>% 
  hc_yAxis(title = list(text = "Points Difference")) %>% 
  hc_tooltip(
    headerFormat = as.character(tags$h4("{point.key}", tags$br())),
    pointFormat = tt,
    useHTML = TRUE,
    backgroundColor = "rgba(247,247,247,0.85)",
    borderColor = "rgba(247,247,247,0.85)",
    shadow = FALSE,
    style = list(color = "black", fontSize = "0.8em", fontWeight = "normal"),
    positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: this.chart.plotTop + 0 }; }"),
    shape = "square"
  ) 

hc
