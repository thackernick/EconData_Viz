
rm(list = ls())
####preliminaries####
library(ggplot2)
library(dplyr)
library(tidyverse)
library(fredr)
library(ggThemeAssist)
library(ggthemes)
library(Cairo)
library(hrbrthemes)
library(lubridate)
library(zoo)
library(rio)

start_date <- as.Date("2021-10-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your key')
setwd()


####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")

####Pull Data from FRED####
gdp = fredr('A191RL1Q225SBEA',
             observation_start = start_date, 
             observation_end = end_date)
gdp = gdp[,c(1,3)]
gdp = gdp %>% rename('Date' = date, 
                       'gdp' = value)

data = gdp

data$gdp_now = 0



# Create a new column 'pos' to mark the GDP sign:
# - 'B' for positive GDP values
# - 'R' for negative GDP values
data = data %>% mutate(pos = ifelse(gdp >= 0, "B", "R"))

# Add rows for GDP Nowcast forecasts for specific dates:
# - For 2024-07-01, a forecasted GDP Now value of 3.1 and 'pos' marked as "F"
# - For 2024-10-01, a forecasted GDP Now value of 1.5 and 'pos' marked as "F"
data <- rbind(data, list(Date = as.Date("2024-07-01"), gdp = NA, gdp_now = 3.1, pos = "F"))
data <- rbind(data, list(Date = as.Date("2024-10-01"), gdp = NA, gdp_now = 1.5, pos = "F"))

# Add a new column 'lr' with a constant long-run rate value of 1.8
data$lr = 1.8

# Ensure the 'Date' column is properly formatted as a Date object
data$Date = as.Date(data$Date)

####Plot####
p1 ={

ggplot(data) + 
  geom_hline(yintercept=0, color = 'black',linetype="dashed", size=1) +
  geom_col(aes(x = Date, y = gdp, fill = 'Actual GDP Growth'), position = 'identity', color = 'black', size = 0.5) + 
  geom_col(aes(x = Date, y = gdp_now, fill = 'GDP Growth Forecast'), position = 'identity', color = 'black', size = 0.5) +
    geom_line(aes(x = Date, y = lr, color = 'Long-Run Forecast 1.8%'), linetype="dashed", size = 2) + 
  geom_hline(yintercept= 1.8, color = paint[2],linetype="dashed", size=2) +
     
  scale_fill_manual(name = '',values = c('Actual GDP Growth' = paint[1], 'GDP Growth Forecast' = paint[3])) +
  scale_color_manual(name = '', values = c('Long-Run Forecast 1.8%'= paint[2])) + 
 
  ylab('Annualized Percent Change From Previous Period') + 
  xlab('Source: Bureau of Economic Analysis  |  Grey Shaded Areas Indicate US Recession') + 
  # scale_y_continuous(breaks = seq(-40,40,10)) +
  scale_x_date(date_labels = '%Y') + 
    # coord_cartesian(expand = FALSE) + #turn off axis expansion (padding) 
  theme_ipsum() + 
  theme(
    axis.title.x = element_text(family = font, size = 18, color = 'gray40', 
                                hjust = 0.5, vjust = -3), 
    axis.line = element_line(size = 0.5, linetype = "solid"), 
    axis.title.y = element_text(family = font, size = font_size, color = 'black', 
                                hjust = 0.5, vjust = 5),
    axis.text.x = element_text(colour = "black", angle = 0, size = font_size, 
                               hjust = 0.5), 
    axis.text.y = element_text(color = 'black', size = font_size), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
     
    plot.subtitle = element_text(family = font, size = 18, color = 'gray40', 
                                 vjust = -3.5, hjust = 0),
  legend.position = c(.40, .88), 
  legend.text = element_text(family = font, color = 'black', size = 20))
  
}

p1

add_rec_shade<-function(st_date,ed_date,shade_color="darkgray")
{
  library(fredr)
  library(ecm)
  library(ggplot2)
  fredr_set_key("16771fbacda7f43f5891870d75bf5391")
  
  st_date<-as.Date(start_date)
  ed_date<-as.Date(Sys.Date())
  
  recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
  
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date
  
  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}
  
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                         aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                         fill=shade_color, alpha=0.5)
    return(rec_shade)
  }
}


p1 = p1 + add_rec_shade()

p1

ggsave(dpi = "retina", plot = p1, "gdp.png", type = "cairo-png",
       width = 12, height = 9, units = "in", bg = 'white')

