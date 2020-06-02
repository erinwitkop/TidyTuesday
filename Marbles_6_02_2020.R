## Tidy tuesday Marble Racing
# Erin M Roberts
# PhD Candidate Univerity of Rhode Island

# Load packages
library(tidyverse)
library(ggridges)
library(hrbrthemes)
library(viridis)

# Get the Data
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# Brainstorm questions to ask
  # Which marbles are the fastest 
  # Is there a correlation between starting pole and average lap time
  # Do some marbles do better at particular track lengths
  # Optimal track length for each marble
  # Which teams have the most points

# Final choice: Do a ridgeline plot with each marble showing the distribution of marble times as track lengths increase !
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  

lap_time <- marbles %>%
  mutate(marble_name = fct_reorder(marble_name, -avg_time_lap)) %>%
ggplot(aes(x = avg_time_lap, y = marble_name, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  labs(title = 'Probability of Lap Times for Racing Marbles Across Races',
       x = "Average Lap Time",
       y = "Marble Name",
       caption = "Erin Roberts, PhD Candidate \n source: VillagerDB") +
  theme_ipsum() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(size="12"),axis.text.y = element_text(size="12"),
    axis.title.x=element_text(size="15"), axis.title.y=element_text(size="15"))

ggsave(filename="Racing_marbles_06_02_2020.png", plot=last_plot(), width=11, height = 9, dpi=400)

