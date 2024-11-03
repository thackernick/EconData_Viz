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

fredr_set_key(YourKey)
start_date <- as.Date("2021-01-01")
end_date <- as.Date(Sys.Date())

####Aesthetics####
font = "Times New Roman"
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")
setwd()

# Retrieve monthly payroll employment data (PAYEMS) from FRED, specifying start and end dates
# 'units = "chg"' requests the data as month-to-month changes
data1 = fredr('PAYEMS', 
              observation_start = start_date, 
              observation_end = end_date, 
              units = 'chg')

# Select: date and value
data1 = data1[, c(1, 3)]

# Rename columns: 'date' to 'Date' and 'value' to 'gains'
# This reflects that we are tracking employment gains or changes
data1 = data1 %>% 
  rename('Date' = date, 'gains' = value) 

# Filter 'data1' to include only records on or after the 'start_date'
data = data1 %>% filter(Date >= start_date)

# Add a new column 'target' to 'data', with a constant target value of 2 for reference
data$target = 2


#### Recession Stuff #####
recessions = read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


rec = recessions %>% filter(Peak >= min(data1$Date) & Trough <= max(data1$Date))





# Calculate a 6-month moving average of the 'gains' column and store it in a new column 'moving_avg'
# - `rollmean` calculates the rolling mean with a window of 6
# - `fill = NA` pads the first few rows with NA where there isn't enough data to compute the average
# - `align = 'right'` aligns the moving average to the end of each 6-month period
data <- data %>% 
  mutate(moving_avg = rollmean(gains, 6, fill = NA, align = 'right'))

#### Plot ####
p1 = {
  ggplot(data) + 
    geom_col(aes(x = Date, y = gains, fill = 'Monthly Job Gains'), position = 'identity', color = 'black', size = 0.5) + 
    geom_line(aes(x = Date, y = moving_avg, color = '6-Month Moving Average'), size = 1.25) + 
    scale_fill_manual(name = '', values = c('Monthly Job Gains' = paint[1])) +
    scale_color_manual(name = '', values = c('Monthly Job Gains' = paint[1],'6-Month Moving Average' = paint[3])) + 
    ylab('Thousands of Jobs') + 
    xlab('Source: Bureau of Economic Analysis | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(0, 900, 150), limits = c(0, NA)) +
    scale_x_date(date_labels = '%Y') +  
    coord_cartesian(expand = FALSE) + #turn off axis expansion (padding)
    theme_ipsum() + 
    theme(
      axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                  hjust = 0.5, vjust = -3), 
      axis.line = element_line(size = 0.5, linetype = "solid"), 
      axis.title.y = element_text(family = font, size = 22, color = 'black', 
                                  hjust = 0.5, vjust = 5),
      axis.text.x = element_text(colour = "black", angle = 0, size = 22, 
                                 hjust = -0.1), 
      axis.text.y = element_text(color = 'black', size = 14), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = c(.80, .75), 
      legend.text = element_text(family = font, color = 'black', size = 18))
}


plot(p1)



ggsave(dpi = "retina", plot = p1, "Monthly_Gains.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
