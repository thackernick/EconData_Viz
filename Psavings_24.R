rm(list = ls())

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
library(stringr) #string manipulation

####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")
setwd()

start_date <- as.Date("2018-01-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your key')


# Pull data from (FRED) API
# 'PSAVERT' is the personal savings rate data series
savings = fredr('PSAVERT',
                observation_start = start_date,  # Set start date 
                observation_end = end_date)      # Set end date 

# Select only the first and third columns in the 'savings' data
savings = savings[, c(1, 3)]

# Rename columns 
savings = savings %>% 
  rename('Date' = date,    # Rename 'date' column to 'Date'
         'savings' = value) # Rename 'value' column to 'savings'

# Filter data to include only rows where the Date is on or after 'start_date'
data = savings %>% 
  filter(Date >= start_date)

####recession stuff#### 
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

rec = recessions %>% filter(Peak >= min(data$Date) & Trough <= max(data$Date))


####plot####
 
p1 <- ggplot(data) + 
  geom_rect(data = rec, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'grey', alpha = 0.5) + 
  geom_line(aes(x = Date, y = savings, color = 'Personal Saving Rate'), size = 1.5, linetype = "solid") + 
  ylab('Percent') + 
  xlab('Source: U.S. Bureau of Economic Analysis  | Shaded Area Indicates US Recession') + 
  scale_y_continuous(breaks = seq(0, 40, 5), limits = c(0, NA)) +
  scale_x_date(date_labels = '%Y') +  
  coord_cartesian(expand = FALSE) +  # Turn off axis expansion (padding)
  theme_ipsum() + 
  scale_color_manual(name = '', values = c('Personal Saving Rate' = paint[1])) + 
  theme(
    axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                hjust = 0.5, vjust = -4, margin = margin(t = 15)),  # Added more space below x-axis title
    axis.line = element_line(size = 0.5), 
    axis.title.y = element_text(family = font, size = font_size, color = 'black', 
                                hjust = 0.5, vjust = 5, margin = margin(r = 15)),  # Added more space to the right of y-axis title
    axis.text.x = element_text(colour = 'black', size = font_size - 2, 
                               hjust = 0.2, margin = margin(t = 10)),  # Added space between x-axis text and ticks
    axis.text.y = element_text(color = 'black', size = font_size - 2, margin = margin(r = 10)),  # Added space between y-axis text and ticks
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = font, size = 42, color = 'black', 
                              hjust = 0.1, vjust = 3), 
    legend.position = c(.80, .49), 
    legend.text = element_text(family = font, color = 'black', size = 18)
    
  )

plot(p1)

ggsave(dpi = "retina", plot = p1, "savings_24.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')  
