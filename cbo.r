rm(list = ls())
####preliminaries####
library(readxl)        # To read Excel files if needed
library(tidyr)         # For data reshaping (wide to long format, etc.)
library(ggplot2)       # For data visualization
library(dplyr)         # For data manipulation (filtering, selecting, etc.)
library(tidyverse)     # A meta-package that includes ggplot2, dplyr, and others
library(fredr)         # For accessing Federal Reserve Economic Data (FRED)
library(ggThemeAssist) # For helping with theme adjustments in ggplot2
library(ggthemes)      # Provides extra themes for ggplot2
library(Cairo)         # To handle graphical devices like PNG exports
library(hrbrthemes)    # A collection of ggplot2 themes
library(lubridate)     # For working with dates
library(zoo)           # For time series manipulation

# Set the working directory based on the current RStudio document
setwd()

####Aesthetics####
font = "Times New Roman"   # Font used in the plot
font_size = 22             # Font size for labels
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")  # Custom color palette




start_date <- as.Date("2003-01-01")
end_date <- as.Date(Sys.Date())



 
####Pull data from the 'Figure 6' sheet of the 'cbo_immigration.xlsx' Excel file####
cbo <- read_excel("cbo_immigration.xlsx", sheet = "Figure 6")

# Keep rows starting from the 8th row to the last (skipping header or unwanted rows)
cbo <- cbo %>% slice(8:n())  

# Rename the columns for easier reference
colnames(cbo) <- c("date", "percent")

# Ensure both 'date' and 'percent' columns are numeric
cbo$date <- as.numeric(cbo$date)   # Convert the 'date' column to numeric
cbo$percent <- as.numeric(cbo$percent)  # Convert 'percent' values to numeric

# Calculate the number of rows (n) to determine the date range
n <- nrow(cbo)  

# Generate a sequence of dates starting from January 1, 2000, incrementing yearly
cbo$date <- as.Date(paste0(2000:(2000 + n - 1), "-01-01"))  

# Filter data to only include dates between the specified start and end dates
cbo <- cbo %>% filter(date >= start_date & date <= end_date)



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

recessions_filtered <- recessions %>%
  filter(Peak >= start_date & Trough <= end_date)

####plot####
p1 <- ggplot(cbo) + 
  # Add shaded recession bars
  geom_rect(data = recessions_filtered, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'grey', alpha = 0.5) +
  geom_line(aes(x = date, y = percent), color = paint[1], size = 1.5) +
  ggtitle('Net Immigration') + 
  # Customize labels
  ylab('Millions of People') +
  # xlab('Source:CBO The Demographic Outlook') +
  xlab('') + 
  scale_y_continuous(breaks = seq(0, 3.5, 0.5), limits = c(0, 3.5)) +
  # scale_x_date(breaks = seq(as.Date("2003-01-01"), as.Date("2024-01-01"), by = "4 years"),  date_labels = '%Y') +
    scale_x_date(date_labels = '%Y') + 
  # Set color scheme
  scale_color_manual(name = '', values = c('Net Immigration' = paint[1])) +
  # Apply theme settings
  theme_ipsum() + 
  coord_cartesian(expand = FALSE) +
  theme(
    axis.title.x = element_text(family = font, size = font_size, color = 'gray40', hjust = 0.5, vjust = -3), 
    axis.line = element_line(size = 0.5, linetype = "solid"),
    axis.title.y = element_text(family = font, size = font_size, color = 'black', hjust = 0.5, vjust = 5),
    axis.text.x = element_text(family = font, colour = "black", angle = 0, size = font_size, hjust = -0.1), 
    axis.text.y = element_text(family = font, color = 'black', size = font_size), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = font, size = 28, color = 'black'), 
    legend.position = c(.48, .90)
    
  )

# Plot the graph
plot(p1)


ggsave(dpi = "retina", plot = p1, "cbo.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
