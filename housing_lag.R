
####Preliminaries####
library(readxl)
library(tidyr)
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


setwd()
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2024-08-31")
fredr_set_key('Your Key')


####Zillow market rent####


file_location <- "zillow_rentdata.csv"
zillow_data <- read.csv(file_location)


# Select region
region_name <- "United States"
region_data <- zillow_data %>% filter(RegionName == region_name)

# Extract the rent data 
rent_data <- t(region_data[6:ncol(region_data)])

# Extract the date columns from the column names and remove the "X"
dates <- gsub("X", "", colnames(region_data)[6:ncol(region_data)])

# Convert the date strings into proper Date objects
dates <- as.Date(dates, format = "%m.%d.%Y")

# Convert transposed rent data into a dataframe and add the dates
rent_data <- data.frame(Date = dates, Rent = as.numeric(rent_data))

# calculate YoY percentage change
rent_data <- rent_data %>%
  arrange(Date) %>%
  mutate(YoY_Change = (Rent - lag(Rent, 12)) / lag(Rent, 12) * 100)

rent_data <- rent_data %>%
  filter(Date >= start_date & Date <= end_date)
      


##housing services#
housingpce<- read_excel("data/usxihous.xlsx", sheet= 'Sheet3') 


housingpce$Date <- as.Date(housingpce$Dates, format = "%d-%b-%Y")

housingpce <- housingpce %>%
  arrange(Date) %>%
  mutate(PCE_Change = (LAST_PRICE - lag(LAST_PRICE, 12)) / lag(LAST_PRICE, 12) * 100)


rent_data_selected <- rent_data %>%
  select(Date, YoY_Change_rent = YoY_Change)

housingpce_selected <- housingpce %>%
  select(Date, YoY_Change_pce = PCE_Change)

# Merge the datasets on Date
data_combined <- merge(rent_data_selected, housingpce_selected, by="Date", all=TRUE)

data_combined <- data_combined %>%
  filter(Date >= start_date & Date <= end_date)
data_combined$Date <- as.Date(data_combined$Date)

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
rec = recessions %>% filter(Peak >= start_date & Trough <= end_date)  

         
####plot####
p1 = {
  ggplot(data_combined) + 
    geom_rect(data = rec, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
              fill = 'grey', alpha = 0.5) +
    geom_line(aes(x = Date, y = YoY_Change_rent, color = 'Zillow market rents'), size = 1.5, linetype = "solid") + 
    geom_line(aes(x = Date, y = YoY_Change_pce, color = 'PCE housing services'), size = 1.5, linetype = "solid") + 
    ylab('Percent Change from a Year Ago') + 
    xlab('Source: U.S. Bureau of Economic Analysis, Zillow Observed Rent Index | Shaded Area Indicates US Recession') + 
    scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, NA)) +
    scale_x_date(date_labels = '%Y', limits = c(min(data_combined$Date), max(data_combined$Date)))+  
    theme_classic() + 
    coord_cartesian(expand = FALSE) + 
    scale_color_manual(name = '', values = c('Zillow market rents' = paint[1], 'PCE housing services' = paint[2])) + 
    theme(
      axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                  hjust = 0.5, vjust = -3), 
      axis.line = element_line(size = 0.5), 
      axis.title.y = element_text(family = font, size = font_size, color = 'black', 
                                  hjust = 0.5, vjust = 5),
      axis.text.x = element_text(colour = "black", angle = 0, size = font_size, 
                                 hjust = -0.1), 
      axis.text.y = element_text(color = 'black', size = font_size), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(family = font, size = 42, color = 'black', 
                                hjust = 0.1, vjust=3), 
      legend.position = c(.29, .78), 
      legend.text = element_text(family = font, color = 'black', size = 18),
      legend.background = element_rect(fill = NA, color = NA),  
      plot.margin = margin(t = 30, r = 80, b = 30, l = 60) ) 
}


plot(p1)



ggsave(dpi = "retina", plot = p1, "Housinglag.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')



