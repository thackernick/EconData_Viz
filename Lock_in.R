####Preliminaries####
library(ggplot2)
library(dplyr)
library(tidyverse)
library(fredr)
library(Cairo)
library(lubridate)
library(hrbrthemes)

setwd("")

####Aesthetics####
font = "Times New Roman"
font_size = 22
paint = c("#2c608a", "#b93936", "#89ae43", "#fbc632", "#89cce7")
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2024-01-01")
fredr_set_key("Your key")


####Data Cleaning####
out_mort <- read.csv("data/oustanding_mortgages.csv")
out_mort <- out_mort %>% select(PERIOD, VALUE1)
out_mort <- out_mort %>%
  mutate(DATE = as.Date(paste0(sub("Q", "-", PERIOD), "-01"), format = "%Y-%m-%d"))

# Rename and filter by date
out_mortclean <- out_mort %>%
  select(DATE, VALUE1) %>%
  rename('date' = DATE) %>%
  filter(date >= start_date & date <= end_date)

# Load FRED data and filter
current_mort <- fredr(
  series_id = "MORTGAGE30US",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "q"
)
current_mort <- current_mort %>%
  select(date, value) %>%
  filter(date >= start_date & date <= end_date)

# Ensure the dates in out_mortclean match current_mort
out_mortclean <- out_mortclean %>%
  filter(date %in% current_mort$date)

# Merge datasets and reshape for plotting
data_all <- merge(out_mortclean, current_mort, by = "date", all = TRUE)
data_all <- data_all %>%
  rename(outstanding_mortgages = VALUE1, current_mortgage_rate = value) %>%
  pivot_longer(cols = c(outstanding_mortgages, current_mortgage_rate),
               names_to = "Type",
               values_to = "Value")

# Filter out rows with NA values
data_all <- data_all %>% filter(!is.na(Value))

####recession stuff#### 
recessions <- read.table(textConnection(
  "Peak, Trough
2007-12-01, 2009-06-01
2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

rec <- recessions %>% filter(Peak >= min(data_all$date) & Trough <= max(data_all$date))

####plot####
p1 <- ggplot(data_all) +
  geom_rect(data = rec, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),
            fill = 'grey', alpha = 0.5) +  # Recession Lines
  geom_line(aes(x = date, y = Value, color = Type), size = 1.5) +
  ylab('Percent') +
  xlab('') +
  labs(caption = 'Source: Freddie Mac | Shaded Area Indicates US Recession') +
  scale_y_continuous(limits = c(min(data_all$Value, na.rm = TRUE), max(data_all$Value, na.rm = TRUE))) +
  scale_x_date(date_labels = '%Y', 
               breaks = seq(as.Date("2018-01-01"), max(data_all$date, na.rm = TRUE), by = "2 year"))+
  coord_cartesian(expand = FALSE) +  # Turn off axis expansion (padding)
  theme_ipsum() +
  scale_color_manual(name = '',
                     values = c('outstanding_mortgages' = paint[1],
                                'current_mortgage_rate' = paint[2]),
                     labels = c('outstanding_mortgages' = 'Average Outstanding Mortgage Rate',
                                'current_mortgage_rate' = '30-Year Fixed Rate Mortgage Average Rate')) +
  theme(
    axis.title.x = element_text(family = font, size = 14, color = 'gray40',
                                hjust = 0.5, vjust = 3),
    axis.line = element_line(size = 0.5, linetype = "solid"),
    axis.title.y = element_text(family = font, size = font_size, color = 'black',
                                hjust = 0.5, vjust = 5),
    axis.text.x = element_text(colour = "black", angle = 0, size = font_size,
                               hjust = 0.2),
    axis.text.y = element_text(color = 'black', size = font_size),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = font, size = 28, color = 'black'),
    legend.position = c(.41, .88),
    legend.text = element_text(family = font, color = 'black', size = 14),
    legend.background = element_rect(fill = NA, color = NA),
    plot.caption = element_text(size = 14, hjust = 0.5, family = font),
    plot.margin = margin(t = 30, r = 50, b = 50, l = 60)
  )

# Display the plot
print(p1)


ggsave(dpi = "retina", plot = p1, "lock_in.png", type = "cairo-png",
       width = 10, height = 7, units = "in", bg = 'white')
