library(worldfootballR)
library(tidyverse)
Bay_Spending <- data.frame(
  season = numeric(),
  in_league_spend = numeric(),
  total_spend = numeric()
)

for (i in 2015:2024) {
  Germany <- tm_league_team_urls(league_url = "https://www.transfermarkt.us/bundesliga/startseite/wettbewerb/L1", 
                                     start_year = i)
  Bay <- Germany[grepl("bayern", Germany)]
  Bay_Transfers <- tm_team_transfers(Bay, transfer_window = "all")
  season<- i
  lg <- Bay_Transfers[1,2]
  in_league <- Bay_Transfers %>% filter(league_2 == lg)
  in_league_spend <- sum(in_league$transfer_fee, na.rm = TRUE)
  total_spend <- sum(Bay_Transfers$transfer_fee, na.rm = TRUE)
  
  Bay_Spending <- rbind(Bay_Spending, data.frame(
    season = season,
    in_league_spend = in_league_spend,
    total_spend = total_spend
    
  ))
  Sys.sleep(5)
}
Bay_Spending$total_spend <- Bay_Spending$total_spend/1000000
Bay_Spending$in_league_spend <- Bay_Spending$in_league_spend/1000000

library(showtext)
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

p <- Bay_Spending %>% ggplot(aes(x=season)) + geom_line(aes(y=total_spend, color="dodgerblue2")) +
                               geom_point(aes(y=total_spend, color="dodgerblue2")) + 
                               geom_line(aes(y=in_league_spend, color="brown1")) +
                               geom_point(aes(y=in_league_spend, color="brown1")) +
  scale_x_continuous(breaks=seq(2015,2024,by=1)) + 
  scale_y_continuous(limits = c(0,190), n.breaks = 19) + 
  labs(x = "Season", y = "Spending", 
       title = "Bayern Munich Spending Within Bundesliga",
       subtitle = ("(in millions of €)"),
       caption = "Created by Rob Gilligan | @RobG195") +
  theme(
    plot.background = element_rect(fill = "antiquewhite1", colour = "antiquewhite1"),
    panel.background = element_rect(fill = "antiquewhite1", colour = "antiquewhite1"),
    panel.grid.major = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "gray0"),
    axis.text = element_text(colour = "gray0"),
    axis.title = element_text(colour = "gray0", size = 13, family = 'Roboto', face = "bold"),
    plot.title = element_text(colour = "gray0", hjust=.5, face="bold", size = 15, family = 'Roboto'),
    plot.caption = element_text(size = 9, hjust = 1.17, family = "Roboto", 
                                face = "bold"),
    legend.background = element_rect(fill = "antiquewhite1", color = NA),
    legend.box.background = element_rect(fill = "antiquewhite1", color = NA),
    legend.text = element_text(colour = "gray0", size = 9, family = 'Roboto', face = "bold"),
    plot.subtitle = element_text(colour = "gray0", hjust=.5, face="bold", size = 10, family = 'Roboto')) +
  labs(color = NULL) + scale_color_manual(values = c("dodgerblue2", "brown1"),
                                         labels = c("Spend in Bundesliga", "Overall Spend")) +
  guides(color = guide_legend(reverse = TRUE))
   
library(cowplot)
logo <- paste0("/Users/robg/Documents/R_Projects/Dominant-Soccer-Team_Spending/Bayern.png")
blogo <- paste0("/Users/robg/Documents/R_Projects/Dominant-Soccer-Team_Spending/blogo.png")
ggdraw() + 
  draw_plot(p) +
  draw_image(
    logo, x = 0.98, y = 0.95, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07) + 
  draw_image(
    blogo, x = 0.98, y = 0.78, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07)

mean(Bay_Spending$in_league_spend/Bay_Spending$total_spend)

  
  
  
  