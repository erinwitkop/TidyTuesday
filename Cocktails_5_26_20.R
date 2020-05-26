## Tidy tuesday Cocktails
# Erin M Roberts
# PhD Candidate Univerity of Rhode Island

### Load libraries ####
library(tidyverse)

#### Get the Data ####

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

## Plotting ingreidents by drink type
cocktails_alc <- cocktails %>% filter(alcoholic == "Alcoholic")

# Most common ingredients
cocktails_alc_count <- cocktails_alc %>% count(ingredient) 
# Several entries that need to be recoded because factor names should be the same but are different

# Recode ingredients that should be the same
unique_ingredient <- unique(cocktails_alc$ingredient)
View(unique_ingredient[order(unique_ingredient)])

# Make all words uppercase because this is most of the issue
cocktails_alc$ingredient <- tolower(cocktails_alc$ingredient)
View(unique(cocktails_alc$ingredient))

# Recode two that should be the same
cocktails_alc$ingredient <- recode(cocktails_alc$ingredient, "anis"="anise", "baileys irish cream"="bailey's irish cream")
View(unique(cocktails_alc$ingredient))

# Ingredients in each category of drink - this is most interesting
cocktails_alc_cat <- cocktails_alc %>% group_by(category) %>% count(ingredient)

# Percent of cocktails with that ingredient
cocktails_alc_cocktails <- cocktails_alc %>% distinct(category, drink) %>% group_by(category) %>% summarise(number_drinks = n())

cocktails_alc_cat_percent <- left_join(cocktails_alc_cat, cocktails_alc_cocktails)

# Calculate percent of drinks with ingredient
cocktails_alc_cat_percent <- cocktails_alc_cat_percent %>% group_by(category, ingredient) %>% summarise(percent_drinks = (n / number_drinks)*100)

# Subset for the top ingredients
cocktails_alc_cat_percent_top <- cocktails_alc_cat_percent %>% filter(percent_drinks >= 20.0)

# Add unique colors for each item
unique(cocktails_alc_cat_percent_top$ingredient)
colorset = c("cider"            = "#9b4d68",
"corona"              = "#5fba47",
"guinness stout"      = "#9558d2",
"lager"              = "#a9b735",
"gin"                 = "#5970d8",
"vodka"               = "#d69a33",
"bailey's irish cream"= "#cc46ae",
"coffee"        = "#4ec283",
"whipped cream"   = "#e95795",
"food coloring"    = "#37803d",
"sugar"              = "#d181de",
"vanilla extract"    = "#627f2a",
"water"               = "#994c9b",
"godiva liqueur"      = "#8cba78",
"kahlua"         = "#ba3769",
"milk"              = "#42c3c2",
"vanilla ice-cream"   = "#d03d46",
"amaretto"           = "#3f906f",
"ice"               = "#d8602e",
"orange juice"       = "#5f9cd7",
"sour mix"            = "#a25331",
"red wine"      = "#6c66aa",
"rum"                 = "#b4aa5c",
"coca-cola"           = "#c78dc6",
"root beer"           = "#7f6c29",
"soda water"          = "#e18084",
"surge"               = "#d79565")

cocktails_alc_cat_percent_top$category <- factor(cocktails_alc_cat_percent_top$category, levels=c("Ordinary Drink" ,"Shot",'Cocktail', "Beer","Coffee / Tea",
"Other/Unknown","Milk / Float / Shake" ,"Punch / Party Drink" ,"Homemade Liqueur", "Soft Drink / Soda"))

ggplot(cocktails_alc_cat_percent_top, aes(x=category, y=percent_drinks , fill=ingredient)) + geom_bar(position = "fill", stat="identity") + 
  geom_text(aes(label=ingredient), stat="identity", position=position_fill(vjust=0.5)) +
  #annotate(cocktails_alc_cat_percent_top$ingredient,label=cocktails_alc_cat_percent_top$ingredient) +
labs(title = "Top 20% of Drink Ingredients for Each Occasion",
       caption = "Erin Roberts, PhD Candidate \n source: Kaggle",
     x = "Drink Category", y = "Percent of Drinks") + 
  scale_fill_manual(values=colorset) +
theme(legend.text = element_text(size=14), text=element_text(size=14), plot.title = element_text(size=18),
       panel.background = element_blank(), legend.position = "bottom", legend.title = ) + 
  scale_y_continuous(labels=scales::percent_format(), expand=c(0,0)) +
  scale_x_discrete(position="top") 
  

ggsave(filename="Cocktails_5_26_2020.png", width=15, height=7)


