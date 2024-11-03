####preliminaries####
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
start_date <- as.Date("2018-01-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your Api Key')

####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")


setwd()


####Pulling data from FRED####

# 'RIFSPFFNB' is the FFR data series; set to monthly frequency and 'end of period' aggregation
ffr = fredr('RIFSPFFNB',
            observation_start = start_date, 
            observation_end = end_date,
            frequency = 'm',
            aggregation_method = 'eop')

# Select only the first and third columns (assumed to be 'date' and 'value') in 'ffr'
ffr = ffr[, c(1, 3)]

# Rename columns for clarity
ffr = ffr %>% rename('Date' = date,     # Rename 'date' column to 'Date'
                     'ffr' = value)     # Rename 'value' column to 'ffr'

# expected inflation data from FRED
# 'T5YIE' is the 5-Year Breakeven Inflation Rate, with monthly frequency and 'end of period' aggregation
exp_inf = fredr('T5YIE', 
                observation_start = start_date,
                observation_end = end_date,
                frequency = 'm',
                aggregation_method = 'eop')

# Select only the date and value columns
exp_inf = exp_inf[, c(1, 3)]

# Rename columns
exp_inf = exp_inf %>% rename('Date' = date,       # Rename 'date' to 'Date'
                             'exp_inf' = value)   # Rename 'value' to 'exp_inf'

# Merge the FFR and expected inflation data by 'Date'
data <- merge(ffr, exp_inf)

# Create a new column 'rffr' in 'data' by subtracting 'exp_inf' from 'ffr' (real federal funds rate)
data <- data %>% mutate(rffr = ffr - exp_inf)


# 'FEDTARMDLR' is the r-star series with monthly observations
rstar = fredr('FEDTARMDLR', 
              observation_start = start_date,
              observation_end = end_date)

# Select only the date and value columns
rstar = rstar[, c(1, 3)]

# Rename columns for clarity
rstar = rstar %>% rename('Date' = date,    # Rename 'date' to 'Date'
                         'rstar' = value)  # Rename 'value' to 'rstar'

# Adjust r-star by subtracting 2 to obtain a custom adjusted r-star value
rstar = rstar %>% mutate(rstar = rstar - 2)

# Convert 'rstar' data to a zoo object with date format in year-quarter format
z <- read.zoo(rstar, FUN = function(x) as.yearqtr(rstar$Date, "%Y-Q%q"))

# Create a monthly time sequence based on the start and end of 'z'
tt <- seq(as.yearmon(start(z)), as.yearmon(end(z)), 1/12)

# Aggregate 'z' by month, applying a custom function to handle missing values
z.ym <- aggregate(z, as.yearmon, c)

# Fill missing monthly values
Value <- na.approx(merge(z.ym, zoo(, tt)))

# Convert the interpolated zoo object back into a data frame
rstar_mon <- fortify.zoo(Value)

# Get the number of rows in 'rstar_mon' and add the last available row with the August 2022 date
rstarm_size <- nrow(rstar_mon)
last_row <- data.frame(as.yearmon("Aug 2022"), rstar_mon$Value[rstarm_size])

# Rename columns in the last row to match 'rstar_mon'
names(last_row) <- c("Index", "Value")

# Append 'last_row' to 'rstar_mon' and rename columns
rstar_mon <- rbind(rstar_mon, last_row)
names(rstar_mon) <- c("Date", "rstar")

# Convert 'Date' to Date class
rstar_mon$Date <- as.Date(rstar_mon$Date)

# Merge the adjusted r-star data with 'data' by 'Date' column
data = merge(data, rstar_mon, by = 'Date')

# Add a column called 'zero' to 'data' with a constant value of 0 (used as a reference line in plots)
data$zero = 0




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
    geom_hline(yintercept = 0, color = 'grey', size = 1.5, linetype = 'dotted') + 
    geom_line(aes(x = Date, y = rffr, color = 'Real FFR (FFR - Expected 5yr Inflation)'), 
              size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = rstar, color = 'R-Star (Longer-Run FFR Forecast - 2%)'), 
              size = 1.5, linetype = "dashed") + 
    ylab('Percent') + 
    xlab('Source: Board of Governors, US Treasury Department, FOMC  | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(-3, 4, 1), limits = c(-3.4, 4)) +
    scale_x_date(date_labels = '%Y') +  
    coord_cartesian(expand = FALSE) + # Turn off axis expansion (padding)
    theme_classic() + 
    scale_color_manual(name = '', values = c('Real FFR (FFR - Expected 5yr Inflation)' = paint[1], 
                                             'R-Star (Longer-Run FFR Forecast - 2%)' = paint[2]), 
                       breaks = c('Real FFR (FFR - Expected 5yr Inflation)', 'R-Star (Longer-Run FFR Forecast - 2%)')) + 
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
                                hjust = 0.1, vjust = 3), 
      legend.position = c(.30, .9), 
      legend.text = element_text(family = font, color = 'black', size = 17),
      legend.background = element_rect(fill = alpha('white', 0)), # Transparent background
      legend.key = element_blank(), # Transparent keys
      plot.margin = margin(t = 10, r = 10, b = 30, l = 20)
    )
}


plot(p1)


ggsave(dpi = "retina", plot = p1, "real_ffr_rstar.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
