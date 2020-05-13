## Tidy tuesday Volcanoes
# Erin M Roberts
# PhD Candidate Univerity of Rhode Island

## Load packages 
library(tidyverse)
library(maps)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(htmltools)
library(plotly)
library(viridis)
library(mapproj)
library(extrafont)

# Get the Data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

## Explore the data 
summary(volcano)
summary(eruptions)
summary(events) #note that events are not just eruptions, they include other events

## explore potential questions
# 1. which type of volcano is most frequent?
# 2. What type of tectonic setting is most frequently associated with a volcano?
# 3. Which region has the most volcanoes
# 4. What is the most common major rock for each region?
# 5. Does the length of time of an eruption vary by latitude and longitude? - this would be good on a map!
# 6. Frequency of lahar by country - LETS GO WITH THIS ONE

# Add lat and long for volcano onto the events
events_lat_long <- left_join(events, unique(eruptions[,c("volcano_name","latitude","longitude")]))

# Add on region information
events_lat_long_region <- left_join(events_lat_long, unique(volcano[,c("volcano_name","region","subregion","country")]))

# Notice there are quite a few NAs in the region, subregion, and country category...
# Try to fill in missing data for country usin maps package
events_lat_long_region$country
events_lat_long_region_NA <- events_lat_long_region %>% filter(is.na(country))
# map to coutnry using map.where, must put longitude first
events_lat_long_region_NA$country <- map.where(database = "world", x=events_lat_long_region_NA$longitude, y =events_lat_long_region_NA$latitude)
# remove NAs 
events_lat_long_region_NA <- events_lat_long_region_NA %>% filter(!is.na(country))

# Split country column so everything after the semi colon is in subcountry category
events_lat_long_region_NA <- separate(events_lat_long_region_NA, country, into=c("country","subcountry"), sep=":" )

# Add back to full dataset 
events_lat_long_region_country <- left_join(events_lat_long_region, unique(events_lat_long_region_NA[,c("volcano_name","country")]))

# remove rows with no country designation
events_lat_long_region_country <- events_lat_long_region_country %>% filter(!is.na(country))
#check for duplicate rows
events_lat_long_region_country[duplicated(events_lat_long_region_country),] # 0 duplicated rows, each row has unique event

# filter for lahar
events_lat_long_region_country_lahar <- events_lat_long_region_country %>% filter(!is.na(event_type)) %>% filter(event_type == "Lahar or mudflow")
events_lat_long_region_country_lahar %>% group_by(volcano_name) %>% summarise(number = n()) %>% View()

# Calculate lahar number by country
events_lat_long_region_country_lahar_number <- events_lat_long_region_country_lahar %>% group_by(country) %>% summarise(lahar_number = n())

# calculate event number
events_lat_long_region_country_total_events <- events_lat_long_region_country %>% group_by(country) %>% summarise(event_number_by_country = n()) 

# Join by country
events_lat_long_region_country_lahar_number_total <- full_join(events_lat_long_region_country_lahar_number, events_lat_long_region_country_total_events, by ="country")

# replace 0's
events_lat_long_region_country_lahar_number_total$lahar_number <- replace_na(events_lat_long_region_country_lahar_number_total$lahar_number, "0")
summary(events_lat_long_region_country_lahar_number_total)

# change to numeric
events_lat_long_region_country_lahar_number_total$lahar_number <- as.numeric(events_lat_long_region_country_lahar_number_total$lahar_number)

# Calculate frequency of lahars by countries
lahar_frequency <- events_lat_long_region_country_lahar_number_total %>% mutate(lahar_frequency = (lahar_number / event_number_by_country)*100)
  
#### Create chloropleth map in leaflet showing lahar data ####

# Thanks to https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html for the very helpful tutorial!

# download world shape file
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="/Users/erinroberts/Documents/Tidy_tuesday/world_shape_file.zip")

# Unzip
system("unzip /Users/erinroberts/Documents/Tidy_tuesday/world_shape_file.zip")

# Read in shape file with the rgdal
world_shape_file <- readOGR( 
  dsn= paste0("/Users/erinroberts/Documents/Tidy_tuesday/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Make data frame with my data
world_shape_file@data <- world_shape_file@data[,-7]

# change country column name to name
colnames(lahar_frequency)[1] <- "NAME"
world_shape_file@data <- left_join(world_shape_file@data, lahar_frequency[,c("NAME","lahar_frequency")])

# Change NAs for lahar frequency 
world_shape_file@data[is.na(world_shape_file@data$lahar_frequency),] <- "0"
world_shape_file@data$lahar_frequency <- as.numeric(world_shape_file@data$lahar_frequency)

# Make map with custom bins
# Create a color palette with handmade bins.
mybins <- c(0,1,2,3,4,5,6,7)
colors_bins <- colorBin( palette="GnBu", domain=world_shape_file@data$lahar_frequency, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_shape_file@data$NAME, "<br/>", 
  "Lahar Frequency: ", world_shape_file@data$lahar_frequency, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_shape_file) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~colors_bins(lahar_frequency), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=colors_bins, values=~lahar_frequency, opacity=0.9, title = "Lahar Frequency (%)", position = "bottomleft" )

## Colors aren't showing up because the country is so small!

#### Plotting in ggplotly ###
# load world polygon
world_map <- map_data("world")
colnames(lahar_frequency)[1] <- "region"

# get one lat and long set per country 

# Add country lat and long to lahar frequency
lahar_frequency_join <- left_join(lahar_frequency, unique(world_map[,c("lat","long","region")]))
lahar_frequency_join <- lahar_frequency_join[!duplicated(lahar_frequency_join$region),]
lahar_frequency_join <- drop_na(lahar_frequency_join)

# Make custom breaks
mybreaks <- c(0.0, 1, 2.0, 3.0, 4.0, 5.0, 6.0)

# add in fonts
#font_import()
fonts()

# Plot lahar frequency using bubble plot 
 bubble_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group=group), fill="grey", alpha=4) + 
   coord_map(xlim=c(-180,180)) +
    geom_point( data=lahar_frequency_join , aes(x=long, y=lat, size=lahar_frequency, color=lahar_frequency), stroke=FALSE) +
    theme_void() + 
   guides(colour = guide_legend()) +
   scale_size_continuous(name="Lahar Frequency by Country (%)", range=c(1,12),breaks=mybreaks) +
   scale_alpha_continuous(name="Lahar Frequency by Country (%)", range=c(0.1, .9),breaks=mybreaks) +
   scale_color_viridis(name="Lahar Frequency by Country (%)" ,breaks=mybreaks) +
   #labs(color="Lahar Frequency by Country (%)")+ 
   theme(plot.title = element_text(size= 18, hjust=0.6, margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
         plot.background = element_rect(fill = "#f5f5f2", color = NA), 
         legend.text = element_text(size=14),
         panel.background = element_rect(fill = "#f5f5f2", color = NA), 
         legend.background = element_rect(fill = "#f5f5f2", color = NA),
         text=element_text(family="Arial", face = "bold")) +
   labs(title = "Lahars are a Small Percentage of Volcanic Events",
      caption = "Erin Roberts, PhD Candidate \n source: Smithsonian Institution")  

ggsave(filename="Volcano_5_12_2020.png")


# Website with helpful information about lahars: https://volcanoes.usgs.gov/vhp/lahars.html
