## Tidy tuesday American Slavery and Juneteenth
# Erin M Roberts
# PhD Candidate Univerity of Rhode Island

# Load packages
library(tidyverse)
library(ggrepel)
library(hrbrthemes)

# Get the Data
blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

# Investigate Blackpast

blackpast %>% group_by(subject) %>% summarize(event_count = n()) %>% View()
blackpast %>% group_by(subject,year) %>% summarize(event_count = n()) %>% View()

# View the top categories since 1950
blackpast %>% group_by(subject,year) %>% filter(year >1950) %>% View()

# Science related accomplishments in the last 100 years 
blackpast %>% 
  group_by(subject) %>% 
  filter(subject == "Exploration and Discovery" | subject == "Health and Medicine" | 
      subject == "Science and Technology") %>% filter(year > 1900) %>% 
  # plotting over time
  ggplot(aes(x=year, y=subject, color=subject,
  # fix the width of the text
  label=str_wrap(events, 50)))+ 
  # add text to plot that repels (ggrepel)
  geom_label_repel(segment.colour = NA, label.size=0, alpha=0.7, fontface="bold",
                   family=font_an, size=3.4) +
  # move x-axis to the top
  scale_x_discrete(position="top")+
  # labels
  labs(x=NULL, y=NULL,
       subtitle = "Data reveals lack of visibility of African-American 21st Century STEM Acheivements",
  caption="Plot by Erin M. Roberts | Source: https://www.blackpast.org/african-american-history-timeline/",
  title="African-American recognized Science Acheivements Since 1900") +
  # add colors for each category
  scale_color_manual(values=c("#7aa456", "#c65999", "#c96d44","#777acd"))+
  # change the theme
  theme_ipsum()+
  theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(size=1, color="gray75"),
          plot.background = element_blank(),
          axis.text = element_text(face="bold", size = 16),
        title = element_text(size=16),
        text = element_text(size=15),
          legend.position="none")
    
# Saving -----------------------------
ggsave("AfricanAmerican_STEM_1900.png", width = 17, height = 7, dpi=400)
  
  