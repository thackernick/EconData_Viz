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
fredr_set_key('YourKey')
setwd()


font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")


# Read in the specified Excel sheet containing the data for Q2 2024
data <- read_excel("HHD_C_Report_2024Q2.xlsx", sheet = "Page 13 Data")

# Rename the columns for easier access and readability
colnames(data) <- c("Date", "AUTO", "CC", "MORTGAGE", "HELOC", "STUDENT", "OTHER", "Total")

# Remove unnecessary header rows (first 4 rows) that don’t contain actual data
data <- data %>% slice(-1:-4) 

# Identify and print the unique dates in the dataset for verification
unique_dates <- unique(data$Date)
print(unique_dates)

# Convert the 'Date' column to character type to allow for substring manipulation
data$Date <- as.character(data$Date)

# Convert the quarterly labels (e.g., "24 Q2") into full dates marking the first day of each quarter
data <- data %>%
  mutate(
    FullDate = as.Date(paste0("20", substr(Date, 1, 2), "-", 
                              ifelse(substr(Date, 4, 5) == "Q1", "01",       # Q1 -> January
                                     ifelse(substr(Date, 4, 5) == "Q2", "04",  # Q2 -> April
                                            ifelse(substr(Date, 4, 5) == "Q3", "07", "10"))), # Q3 -> July, Q4 -> October
                              "-01"))
  )

# Display the first few rows of the updated dataset for verification
head(data)

# Select only the relevant columns (FullDate, AUTO, and CC) for analysis
filtered_data <- data %>%
  select(FullDate, AUTO, CC) 

# Convert FullDate to Date type, and AUTO and CC columns to numeric for accurate filtering and analysis
filtered_data$FullDate <- as.Date(filtered_data$FullDate)
filtered_data$AUTO <- as.numeric(filtered_data$AUTO)
filtered_data$CC <- as.numeric(filtered_data$CC)

# Filter the data to include only dates later than the specified start date
filtered_data <- filtered_data %>% filter(FullDate > start_date)



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


rec = recessions %>% filter(Peak >= min(filtered_data$FullDate) & Trough <= max(filtered_data$FullDate))

p2 <- ggplot(filtered_data) + 
  geom_rect(data = rec, aes(xmin = as.Date(Peak), xmax = as.Date(Trough), ymin = -Inf, ymax = +Inf), 
            fill = 'grey', alpha = 0.5) +
  geom_line(aes(x = FullDate, y = AUTO, color = 'Auto'), size = 1.5, linetype = "solid") +
  geom_line(aes(x = FullDate, y = CC, color = 'Credit Card'), size = 1.5, linetype = "solid") + 
  ggtitle('Delinquency Rates') + 
  ylab('Percent') + 
  #xlab('Source: NY Fed Consumer Credit Panel/Equifax') + 
  xlab('')+
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, NA)) +  
  scale_x_date(date_labels = '%Y', date_breaks = "1 year") +  # Changed to 1 year for more frequent labels
  theme_ipsum() +
  coord_cartesian(expand = FALSE) + 
  scale_color_manual(name = '', values = c('Auto' = paint[1], 
                                           'Credit Card' = paint[2])) + 
  theme(
    axis.title.x = element_text(family = font, size = 14, color = 'gray40', 
                                hjust = 0.5, vjust = -3), 
    axis.line = element_line(size = 0.5, linetype = "solid"), 
    axis.title.y = element_text(family = font, size = font_size, color = 'black', 
                                hjust = 0.5, vjust = 5),
    axis.text.x = element_text(colour = "black", angle = 0, size = font_size, 
                               hjust = 0.5), 
    axis.text.y = element_text(color = 'black', size = font_size), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = font, size = 28, color = 'black'), 
    legend.position = c(.77, .33), 
    legend.text = element_text(family = font, color = 'black', size = font_size)
    #legend.background = element_rect(fill = NA, color = NA),
   # plot.margin = margin(t = 30, r = 30, b = 50, l = 90)
  )

plot(p2)
ggsave(dpi = "retina", plot = p2, "credit_card_del.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
