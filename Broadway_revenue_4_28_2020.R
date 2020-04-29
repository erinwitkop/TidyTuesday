# TidyTuesday Apr. 28th, 2020
# Erin Michele Roberts, PhD Candidate Biological and Environmental Science
# University of Rhode Island, Marta Gomez-Chiarri Lab

####  Load libraries ####
library(tidyverse)
library(bbplot)
library(lubridate)
library(ggpubr)
library(hrbrthemes)

## Download data from Alex Cookson
# Get the Data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

### Inspect the data 
# Dates for both in YYYY-MM-DD format
# Find date range of broadway shows
grosses_date_range <- range(grosses$week_ending) # 1985 to 2020
# "1985-06-09" "2020-03-01"

# The CPI is per month, dates also in YYYY-MM-DD format 
View(cpi)

# Calculate monthly average grosses using floor_date with lubridate
grosses_per_month <- grosses %>% group_by(month=floor_date(week_ending, "month")) %>% summarise(monthly_gross_overall = mean(weekly_gross_overall))
colnames(grosses_per_month )[1] <-"year_month"

# Join cpi by month
grosses_per_month_cpi <- left_join(grosses_per_month, cpi)

# Adjust monthly average gross overall using procedure here: https://towardsdatascience.com/the-what-and-why-of-inflation-adjustment-5eedb496e080
# divide by CPI and multiple by 100
class(grosses_per_month_cpi$monthly_gross_overall) # numeric
class(grosses_per_month_cpi$cpi) # numeric
class(grosses_per_month_cpi$year_month) # Date
class(grosses_per_month_cpi)
grosses_per_month_cpi_adj <- grosses_per_month_cpi %>% mutate(monthly_avg_gross_adj = (monthly_gross_overall / cpi)*100)

## Plot adjusted values to explore
ggplot(grosses_per_month_cpi_adj, aes(x=year_month, y =monthly_avg_gross_adj)) + geom_line() 
## Plot for a single year
grosses_per_month_cpi_adj_2011 <- grosses_per_month_cpi_adj %>% filter(year_month >= as.Date("2011-01-01") & year_month <= as.Date("2015-01-01"))
ggplot(grosses_per_month_cpi_adj_2011, aes(x=year_month, y =monthly_avg_gross_adj)) + geom_line() + geom_point()

## Which months have the yearly max and min? Curious about patterns in months
grosses_per_month_cpi_adj_max <- grosses_per_month_cpi_adj %>% group_by(Year = floor_date(year_month, "year")) %>% summarise(monthly_avg_gross_adj = max(monthly_avg_gross_adj) )
# January, December, and April all seem to be popular maximums
grosses_per_month_cpi_adj_min <- grosses_per_month_cpi_adj  %>% group_by(Year = floor_date(year_month, "year")) %>% summarise(monthly_avg_gross_adj = min(monthly_avg_gross_adj) )
# September and February are the worst months  

# join max and min back to find dates of max and min
grosses_per_month_cpi_adj_max_join <- left_join(grosses_per_month_cpi_adj_max, grosses_per_month_cpi_adj[,c("year_month","monthly_avg_gross_adj")])
colnames(grosses_per_month_cpi_adj_max_join)[3] <- "max_date"
grosses_per_month_cpi_adj_min_join <- left_join(grosses_per_month_cpi_adj_min, grosses_per_month_cpi_adj[,c("year_month","monthly_avg_gross_adj")])
colnames(grosses_per_month_cpi_adj_min_join)[3] <- "min_date"

# Find the difference between max and min for each year
grosses_per_month_cpi_adj_range <- grosses_per_month_cpi_adj %>% group_by(Year = floor_date(year_month, "year")) %>% mutate(difference = max(monthly_avg_gross_adj)-min(monthly_avg_gross_adj))

### Plot the difference over time 
 ggplot(grosses_per_month_cpi_adj_range, aes(x=Year, y =difference)) + 
  geom_line(color="#37547b") +
   geom_point(color="#37547b") +
   geom_smooth(method="lm") +
  stat_cor(label.x = 5000, label.y = 6000000) +
  stat_regline_equation(aes(x=Year, y =difference),label.x = 5000, label.y = 6500000) +
   theme_ipsum(grid="Y") + 
   #scale_x_continuous(breaks=seq()) + 
   theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
         axis.title.x=element_text(size="15"), axis.title.y=element_text(size="15")) +
 scale_y_continuous(labels=scales::dollar_format()) +
   #scale_x_date(date_breaks = "5 years") +
   labs(x="U.S. Dollars", y = "Year",
        title="Broadway Demand is More Volatile Than Ever",
        subtitle="Seasonal swings in monthly average gross are significantly increasing over time")

