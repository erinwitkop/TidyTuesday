## Tidy tuesday Animal Crossing
# Erin M Roberts
# PhD Candidate Univerity of Rhode Island

### Load libraries ####
library(tidyverse)

#### Get the Data ####
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#### Explore the data ####

View(critic)
View(user_reviews)
View(items)
View(villagers )

#### EXPLORE ITEMS ####

class(items$buy_value)
# Explore sell value by category 
items_avg_price <- items %>% group_by(category) %>% filter(!is.na(buy_value)) %>% dplyr::summarise(avg_buy_value = mean(buy_value))

# plot to investigate
ggplot(items_avg_price , aes(x=category, y =avg_buy_value)) + geom_col() + coord_flip()
# hats, furniture, bugs, usables

## Profits made for selling various items
profit_margins <- items_na_rm %>% filter(!is.na(buy_value)) %>%  filter(!is.na(sell_value)) %>% mutate(profit_margin = sell_value - buy_value)

# explore profits you can make
ggplot(profit_margins, aes(x=reorder(category,profit_margin), y =profit_margin)) + 
  geom_point(aes(color=)) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_light() + 
  ylab("Revenue Loss") +
  xlab("Item Category") + 
  ggtitle("")

# Average is affected heavily by outlier items 
avg_profit_margins <- profit_margins %>% group_by(category) %>% summarise(avg_profit_margin = mean(profit_margin), sd_profit_margin = sd(profit_margin))
ggplot(avg_profit_margins, aes(x=reorder(category,avg_profit_margin), y =avg_profit_margin)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_light() + geom_errorbar(aes(ymin=avg_profit_margin-sd_profit_margin, ymax=avg_profit_margin+sd_profit_margin), width=.2,
                                position=position_dodge(.9)) 

#Median profit margins less affected by outliers
median_profit_margins <- profit_margins %>% group_by(category) %>% summarise(median_profit_margin = median(profit_margin))
median_profit_margins_plot <- ggplot(median_profit_margins, aes(x=reorder(category,median_profit_margin), y =median_profit_margin, fill=median_profit_margin)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  ylab("Difference Between Seller Value and Buyer Value") +
  xlab("Item Type") + 
  labs(title = "Animal Crossing Items Depreciate in Value",
       subtitle="Difference between purchase price and seller value is highest for furniture and music") +
  scale_fill_gradient2(high ="indianred1", mid ="indianred2",low ="indianred4" ) +
  theme(legend.position = 'none', text = element_text(size=14))
  
# Items you can make a profit on
# What to buy to make a profit
profit_margins_pos <- profit_margins %>% filter(profit_margin >= 0.0)
profit_margins_pos_plot <- ggplot(profit_margins_pos, aes(x=reorder(name,profit_margin), y =profit_margin, fill=profit_margin)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar_format(), breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000,4500)) +
  theme_minimal() +
  scale_fill_gradient2(low ="dodgerblue1", mid ="skyblue",high ="darkblue" ) +
  theme(legend.position = 'none', text = element_text(size=14)) +
  labs(title = "Select Items Where Sellers Can Make Profits",
       caption = "Erin Roberts, PhD Candidate \n source: VillagerDB",
       y = "Difference Between Seller Value and Buyer Value",
       x='Item Name')

cowplot::plot_grid(median_profit_margins_plot,profit_margins_pos_plot, align="hv", nrow = 2)
ggsave(filename="Animal_crossing.png", plot=last_plot(), width=11, height = 9, dpi=400)


