# Astronauts tidy tuesday
# Erin M Roberts
# July 14th, 2020

# load libraries
library(tidyverse)
library(ggrepel)
library(ggdark)


# load data 
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# Astronauts with the most time in space with the highest mission hours
astronauts %>%
  group_by(name, nationality) %>% 
  # calculate the total hours for each shuttle
  summarize(total_hours_per_person = sum(hours_mission)) %>% 
  ggplot(aes(x=nationality, y=total_hours_per_person)) + 
    geom_point( color = "#eade23")  +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Hours in Space Per Astronaut",
       subtitle = "Russian Astronauts have Spent the Most Time in Space",
       caption = "Erin Roberts, PhD Candidate \n source: DOI: 10.17632/86tsnnbv2w.1",
       x= "Astronaut Nationality", y = "Total Hours")  +
  #geom_label_repel(data = . %>% filter(total_hours_per_person == max(total_hours_per_person)), aes(label = name), size=2) +
  theme(legend.position="none",
        axis.text.x = element_text(size=14, colour = "white"),
        axis.text.y = element_text(size=14, colour ="white"),
        axis.title.x =  element_text(size=16),
        axis.title.y =  element_text(size=16),
        title = element_text(colour ="white"),
        panel.background = element_rect("#2a5fa0"),
        plot.background = element_rect("black"),
        panel.grid.minor = element_blank())

ggsave(filename="Astronauts_07_14_2020.png", width = 10, height = 8)

  