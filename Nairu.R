# Unemployment Rate 
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
start_date <- as.Date("2018-01-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your Key')

####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")
setwd()



####Pull Data from FRED####
ur = fredr('UNRATE',
           observation_start = start_date, 
           observation_end = end_date)
ur = ur[,c(1,3)]
ur = ur %>% rename('Date' = date, 
                   'ur' = value)
nr = fredr('UNRATEMDLR', 
           observation_start = start_date,
           observation_end = end_date)
nr = nr[,c(1,3)]
nr = nr %>% rename('Date' = date, 
                   'nr' = value)
nr$Date = ymd(format(nr$Date, "%Y-%m-01"))


# Convert the 'nr' dataset to a quarterly zoo object ('z') with dates formatted as year-quarter (e.g., "2022-Q3")
z <- read.zoo(nr, FUN = function(x) as.yearqtr(nr$Date, "%Y-Q%q"))

# Create a sequence of monthly dates from the start to the end of the quarterly data ('z')
# 'tt' will include each month in the range as year-month objects
tt <- seq(as.yearmon(start(z)), as.yearmon(end(z)), 1/12)

# Aggregate the quarterly data ('z') to monthly frequency, summing values within each month
z.ym <- aggregate(z, as.yearmon, c)

# Interpolate missing monthly values in 'z.ym' using linear interpolation across the 'tt' time sequence
# This fills in missing months to create a continuous time series
Value <- na.approx(merge(z.ym, zoo(, tt)))

# Convert the interpolated zoo object ('Value') into a data frame ('nr_mon') for easier handling in data frames
nr_mon <- fortify.zoo(Value)

# Rename the columns of 'nr_mon' for clarity: first column to "Date" and the second to "nr"
names(nr_mon) <- c("Date", "nr")

# Convert 'Date' column to Date type, making it consistent for merging with other datasets
nr_mon$Date <- as.Date(nr_mon$Date)

# Merge 'nr_mon' with another dataset 'ur' by the 'Date' column to align data from both sources by date
data = merge(ur, nr_mon, by = 'Date')


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






####Plot####
p1 = {

  ggplot(data) + geom_rect(data = rec, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
                           fill = 'grey', alpha = 0.5)+ 
    geom_line(aes(x = Date, y = ur, color = 'Unemployment Rate'), size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = nr, color = 'NAIRU (Longer-Run FOMC Projection)'), size = 1.5, linetype = "dashed") + 
    #ggtitle('Unemployment Rate vs. NAIRU') + 
    ylab('Percent') + 
    xlab('Source: Bureau of Labor Statistics, Board of Governors  | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(0,15, 5), limits=c(-0.5,15)) +
    scale_x_date(date_labels = '%Y') +  
    coord_cartesian(expand = FALSE) + #turn off axis expansion (padding)
    theme_ipsum() + 
    scale_color_manual(name = '', values = c('Unemployment Rate' = paint[1],
                                             'NAIRU (Longer-Run FOMC Projection)' = paint[2]), breaks=c('Unemployment Rate', 'NAIRU (Longer-Run FOMC Projection)')) + 
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
      plot.title = element_text(family = font, size = 42, color = 'black', 
                                hjust = 0.1, vjust=3), 
      legend.position = c(.77, .8), 
      legend.text = element_text(family = font, color = 'black', size = 18))
  
}


# Adjust the legend to display in two rows for better readability or to save space
p1 = p1 + guides(colour = guide_legend(nrow = 2))

# Ensure that the y-axis includes zero as a minimum limit, expanding the y-axis range if necessary
p1 = p1 + expand_limits(y = 0)

plot(p1)



ggsave(dpi = "retina", plot = p1, "UR_NAIRU.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
