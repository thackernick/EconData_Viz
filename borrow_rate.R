# Borrowing Rates

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
start_date <- as.Date("2018-01-01")
end_date <- as.Date(Sys.Date())
fredr_set_key('Your Key')
setwd()

####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")


####Pull Data from FRED####

# 10-Year Treasury yield 
tenyr = fredr('WGS10YR', 
              observation_start = start_date, 
              observation_end = end_date)

# Select only date and value
tenyr = tenyr[, c(1, 3)]

# Rename columns : 'date' to 'Date' and 'value' to 'tenyr'
tenyr = tenyr %>% rename('Date' = date, 
                         'tenyr' = value)

# Create a new column 'Date_week' in 'tenyr' that represents the year and week number (format: "YYYY-UU")
tenyr$Date_week <- format(tenyr$Date, "%Y-%U")


#  30-Year Mortgage 
mort = fredr('MORTGAGE30US', 
             observation_start = start_date, 
             observation_end = end_date)


mort = mort[, c(1, 3)]

# Rename columns  'date' to 'Date_mort' and 'value' to 'mort'
mort = mort %>% rename('Date_mort' = date, 
                       'mort' = value)

# Create a 'Date_week' column in 'mort' representing the year and week number
mort$Date_week <- format(mort$Date_mort, "%Y-%U")

# Merge 'tenyr' and 'mort' data by 'Date_week' to align weekly data
data = full_join(tenyr, mort, by = 'Date_week')


# Effective Federal Funds Rate (EFFR) 
effr = fredr('DFF', 
             observation_start = start_date, 
             observation_end = end_date)


effr = effr[, c(1, 3)]

# Rename columns: 'date' to 'Date_effr' and 'value' to 'effr'
effr = effr %>% rename('Date_effr' = date, 
                       'effr' = value)

# Create a 'Date_week' column in 'effr' representing the year and week number
effr$Date_week <- format(effr$Date_effr, "%Y-%U")

# Join 'effr' data with the previously merged data on 'Date_week'
data = full_join(data, effr, by = 'Date_week')

# Filter the 'data' dataframe to include only records where 'Date' is later than 'start_date'
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
    geom_line(aes(x = Date, y = effr, color = 'Effective Federal Funds Rate'), size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = tenyr, color = '10-Year Treasury Yield'), size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = mort, color = 'Avg. 30-Year Mortgage Rate'), size = 1.5, linetype = "solid") + 
    
    #geom_hline(yintercept=0, linetype="dashed") + 
    #    ggtitle('Supply Chain Pressures Remain') + 
    ylab('Percent') + 
    xlab('Source: Freddie Mac, Board of Governors | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(0,8, 2), limits=c(-0.1,NA)) +
    scale_x_date(date_labels = '%Y') +  
    coord_cartesian(expand = FALSE) + #turn off axis expansion (padding)
    theme_ipsum() + 
    scale_color_manual(name = '', values = c('10-Year Treasury Yield' = paint[2], 'Avg. 30-Year Mortgage Rate' = paint[1], 'Effective Federal Funds Rate'= paint[3]),
                       breaks=c('Avg. 30-Year Mortgage Rate','10-Year Treasury Yield','Effective Federal Funds Rate' )) + 
    theme(
      axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                  hjust = 0.5, vjust = -3), 
      axis.line = element_line(size = 0.5), 
      axis.title.y = element_text(family = font, size = 22, color = 'black', 
                                  hjust = 0.5, vjust = 5),
      axis.text.x = element_text(colour = "black", angle = 0, size = font_size, 
                                 hjust = 0.5), 
      axis.text.y = element_text(color = 'black', size = font_size), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = c(.30, .8), 
      legend.text = element_text(family = font, color = 'black', size = 18),
      legend.background = element_rect(fill = alpha('white', 1)) # Transparent background
    )
}

plot(p1)

ggsave(dpi = "retina", plot = p1, "borrow_rates.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
