# Wages Doc
# NAIRU

rm(list = ls())


####Preliminaries####
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
library(readxl)
start_date <- as.Date("2017-10-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your key')

####Aesthetics####
font = "Times New Roman"
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")
setwd()
font_size = 22

####data cleaning####
#wage growth data, skipping the first row
data <- read_excel("wage-growth-data.xlsx", skip = 1)

# Rename columns : the first column to 'Date' and the 'Overall' column to 'wage'
data = data %>% rename('Date' = ...1, 'wage' = Overall)

# Convert 'wage' column to numeric to ensure it can be used in calculations
data$wage = as.numeric(data$wage)

# Convert 'Date' column to Date type for consistency in time-series 
data$Date = as.Date(data$Date)


# Employment Cost Index (ECI) data from FRED, setting the observation start and end dates
# 'units = "pc1"' specifies percent change from one year ago for this series
eci = fredr('ECIALLCIV', 
           observation_start = start_date, 
           observation_end = end_date, 
           units = 'pc1')

# Select only the relevant columns: date and value
eci = eci[, c(1, 3)]

# Rename columns: 'date' to 'Date' and 'value' to 'eci' for clarity
eci = eci %>% rename('Date' = date, 
                     'eci' = value)

# Adjust 'Date' to always be the first day of each month, allowing monthly alignment
eci$Date = ymd(format(eci$Date, "%Y-%m-01"))


# Convert ECI data to a quarterly zoo object with year-quarter formatting
z <- read.zoo(eci, FUN = function(x) as.yearqtr(eci$Date, "%Y-Q%q"))

# Create a sequence of monthly dates from the start to the end of the quarterly ECI data
tt <- seq(as.yearmon(start(z)), as.yearmon(end(z)), 1/12)

# Aggregate quarterly ECI data to monthly frequency by summing values within each month
z.ym <- aggregate(z, as.yearmon, c)

# Interpolate missing monthly values in 'z.ym' using linear interpolation across the 'tt' time sequence
Value <- na.approx(merge(z.ym, zoo(, tt)))

# Convert the interpolated zoo object ('Value') into a data frame ('eci_mon') for easier handling
eci_mon <- fortify.zoo(Value)

# Rename columns for clarity: first column to "Date" and the second to "eci"
names(eci_mon) <- c("Date", "eci")

# Convert 'Date' column to Date type for consistency
eci_mon$Date <- as.Date(eci_mon$Date)

# Merge the wage data ('data') with the interpolated monthly ECI data ('eci_mon') by the 'Date' column
data = merge(data, eci_mon, by = 'Date')

# Filter 'data' to include records only where 'Date' is later than 'start_date'
data = data %>% filter(Date > start_date)

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


rec = recessions %>% filter(Peak >= min(data$Date) & Trough <= max(data$Date))

#### Plot ####

p1 = {
  
  ggplot(data) + 
    geom_rect(data = rec, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
                fill = 'grey', alpha = 0.5) +
    geom_line(aes(x = Date, y = eci, color = 'Employment Cost Index'), size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = wage, color = 'Wage Growth Tracker'), size = 1.5, linetype = "solid") + 
     

    ylab('Percent Change From Year Ago') + 
    xlab('Source: Federal Reserve Bank of Atlanta, Bureau of Labor Statistics  | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(0,8, 2), limits=c(-0.2,8)) +
    scale_x_date(date_labels = '%Y') +  
    coord_cartesian(expand = FALSE) + #turn off axis expansion (padding)
    theme_ipsum() + 
    scale_color_manual(name = '', values = c('Wage Growth Tracker' = paint[1], 'Employment Cost Index' = paint[2])) + 
    theme(
      axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                  hjust = 0.5, vjust = -3), 
      axis.line = element_line(size = 0.5), 
      axis.title.y = element_text(family = font, size = font_size, color = 'black', 
                                  hjust = 0.5, vjust = 5),
      axis.text.x = element_text(colour = "black", angle = 0, size = font_size, 
                                 hjust = 0.5), 
      axis.text.y = element_text(color = 'black', size = font_size), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = c(.75, .3), 
      legend.text = element_text(family = font, color = 'black', size = 18)) 
  
}

print(p1)

ggsave(dpi = "retina", plot = p1, "wages.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
