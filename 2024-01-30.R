#setwd("/Users/couleauanabelle/Library/CloudStorage/Dropbox/Folders/10_DATA_ANALYST/Portfolio/data_viz/TidyTuesday/2024/2024-01-30/")

# Load packages -----------------------------------------------------------

library(tidyverse) # a meta-package that loads multiple packages together, such as dplyr, ggplot2, tidyr, and others. These packages provide tools for data manipulation, exploration, and visualization.
library(ggstream) #to build a streamchart in ggplot2
library(colorspace)
library(ggtext) #This package enhances the formatting options for text in ggplot2 plots. It allows you to use rich text formatting, including HTML and Markdown, in your ggplot labels and annotations.
library(cowplot)
library(showtext) #This package is used for displaying and rendering text in R graphics. It allows you to use a variety of fonts and styles in your plots.
library(patchwork) #The patchwork package is used for combining multiple ggplots into a single layout. It provides a flexible and convenient way to arrange and display multiple plots in a grid or other configurations.
library(urbnmapr) #This package provides spatial data for U.S. counties. It includes functions for retrieving and working with geographical data, which can be useful for creating maps and conducting spatial analyses focused on U.S. counties.
library(hrbrthemes) #for lollipop graph

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-01-30')

groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

# Load fonts --------------------------------------------------------------

font_add_google("DM Sans", "dm_sans")
showtext_auto()


# Set the theme of the stream plot -----------------------------------------------

theme_set(theme_minimal(base_family = "", base_size = 50))

theme_update(
  plot.title = element_text(
    family = "dm_sans",
    size = 60,
    color = "grey20",
    face = "bold",
    hjust = .2,
    margin = margin(t = 10, l = 0, b = 20, r = 0)
  ),
  plot.subtitle = element_textbox_simple(
    family = "dm_sans",
    size = 55,
    color = "grey20",
    margin = margin(t = 10, l = 20, b = 20, r = 20),
    lineheight = .8
  ),
  plot.caption = element_text(
    family = "dm_sans",
    size = 50,
    color = "grey20",
    hjust = .5,
    margin = margin(t = 20, l = 0, b = 5, r = 0)
  ),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = "snow2", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  strip.text.y = element_blank(),
  legend.position="none",
  plot.margin = margin(rep(20, 4))
)

# Data wrangling  -------------------------------------------


predictions_adj = predictions %>% 
  select(year, shadow) %>%
  group_by(year) %>% 
  summarise(winter = sum(shadow, na.rm = T), 
         early_spring = length(which(shadow == FALSE)))

predictions_adj_long = pivot_longer( predictions_adj,
                                     cols = c(winter,early_spring),
                                     names_to = "predictions_type",
                                     values_to = "predictions_nb")


# Define text ----------------------------------------------------------------


title <- "Groundhog Day Predictions Count"
st <- glue::glue("I counted the number of predictions made between 1886 and 2023
                 in the US & Canada. The number of predictions increased 
                 substantially around the 1990s. The release of the famous movie
                 in 1993, \'Groundhog Day,\' starring Bill Murray, may have attracted
                 a higher number of predictions. The story follows Phil Connors
                 (played by Bill Murray) who is assigned to cover the annual
                 Groundhog Day event in the small town of Punxsutawney, Pennsylvania.")
vertical_lab <- "The famous Groundhog\n Day movie was\n released in 1993."
cap <- "Visualization by Anabelle Couleau  •  Data by Groundhog-day.com via TidyTuesday."


# Stream chart -----------------------------------------------------------------


g <- predictions_adj_long %>% 
  ggplot(
    aes(
      year, predictions_nb, 
      color = predictions_type, 
      fill = predictions_type,
      label = predictions_type
    )
  ) +
  geom_stream(
    geom = "contour",
    #color = "white",
    size = 1.25,
    bw = .45 # Controls smoothness
  ) +
  geom_stream(
    geom = "polygon",
    bw = .45,
    size = 0
  )  +
  scale_x_continuous(breaks=c(1886,1920,1950,1980,2023),
                     labels = c("1886","1920","1950","1980","2023")
                     ) +
  scale_color_manual(
    expand = c(0, 0),
    values = c( "goldenrod1","powderblue"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c( "goldenrod1","powderblue"),
    name = NULL
  ) + 
  labs(title = title,
       subtitle = st,
       caption = cap) +
  annotate("text", x = 2001, y = 8,
           label = "Early Spring",
           hjust=0,
           size=15,
           lineheight=.8,
           fontface="bold",
           color="white") +
  annotate("text", x = 2005, y = -8,
           label = "Winter",
           hjust=0,
           size=15,
           lineheight=.8,
           fontface="bold",
           color="white") + # Vertical segment
  geom_segment(aes(x = 1993, y = 6, xend = 1993, yend = 15),color="grey20") +
  geom_point(aes(x = 1993, y = 15),color = "grey20") +
  annotate("text", x = 1993, y = 20,
           label = vertical_lab,
           hjust=0.5,
           size=15,
           family = "dm_sans",
           color = "grey20",
           lineheight = .3)

g

ggsave("streamchart-groundhog.png", g, 
       width = 16, height = 13, device = ragg::agg_png)


# County map ---------------------------------------------


groundhogs_w = groundhogs |> filter(country == "USA") |> 
  group_by(city, region) |> 
  summarize(pred_count = sum(predictions_count, na.rm = T))

# get cities + county fips
us_cities <- read.csv(file = "uscities.csv")
us_cities = us_cities %>% rename(region = state_name)

us_cities_groundhog <- merge(us_cities, groundhogs_w, by = c("city","region")) %>% 
  select(c("county_fips", "pred_count")) %>% group_by(county_fips) %>% # sum up two cities in North Carolina that belongs to same county
  summarise(pred_count2 = sum(pred_count, na.rm = T))
head(us_cities_groundhog)

# counties form urbnmap package
us_counties <- ggplot() + 
  geom_polygon(data = urbnmapr::counties, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
us_counties
#View(counties)

# convert county_fips in us_cities dataset from character to numeric to allow for join
counties  <- counties %>%  
  mutate(county_fips = as.numeric(county_fips))

groundhogs_data <- left_join(counties, us_cities_groundhog, by = "county_fips",
                             relationship = "many-to-one")  

groundhogs_data$pred_count2[is.na(groundhogs_data$pred_count2)] = 0
#head(groundhogs_data)


groundhogs_map<-groundhogs_data %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = pred_count2)) +
  geom_polygon(color = "#ffffff", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = 'grey88', high = 'orange') +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.height = unit(.2, "in"),
        legend.text=element_text(size=30,
                                 family = "dm_sans",
                                 color = "grey20",
                                 lineheight = .3),
        axis.text.x = element_blank()) +
  labs(title = "US Counties Predictions Count, 1886-2023") +
  scale_color_continuous(guide = guide_colorbar(
    ticks = FALSE))
groundhogs_map

ggsave("groundhogs_map.png", groundhogs_map, device = ragg::agg_png)

# Lollipop chart --------------------------------------------------------------


us_cities_groundhog_bar <- merge(us_cities, groundhogs_w, by = c("city","region")) %>% 
  select(c("county_fips", "pred_count", "region")) %>% group_by(region) %>%
  summarize(pred_count_state = sum(pred_count))

# Reorder the data
us_cities_groundhog_bar <- us_cities_groundhog_bar %>%
  arrange(pred_count_state) %>%
  mutate(region=factor(region, region))

# Plot
lollipop_groundhog <- ggplot(us_cities_groundhog_bar, aes(x=region, y=pred_count_state)) +
  geom_segment(
    aes(x=region, xend=region, y=0, yend=pred_count_state), 
    color=ifelse(us_cities_groundhog_bar$region %in% c("Pennsylvania"), "orange", "grey"), 
    size=ifelse(us_cities_groundhog_bar$region %in% c("Pennsylvania"), 1.3, 0.7)
  ) +
  geom_point(
    color=ifelse(us_cities_groundhog_bar$region %in% c("Pennsylvania"), "orange", "grey"), 
    size=ifelse(us_cities_groundhog_bar$region %in% c("Pennsylvania"), 5, 2)
  ) +
#  theme_ipsum(grid = FALSE) +
  coord_flip() +
  theme(
    legend.position="none",
    axis.text.x = element_text(
      family = "dm_sans",
      size = 50,
      color = "grey20",
      face = "bold",
      hjust = .2,
      margin = margin(t = 10, l = 0, b = 20, r = 0)),
    axis.text.y = element_text(
      family = "dm_sans",
      size = 50,
      color = "grey20",
      face = "bold",
      hjust = .2,
      margin = margin(t = 10, l = 0, b = 20, r = 0)),
    plot.title = element_text(
      family = "dm_sans",
      size = 60,
      color = "grey20",
      face = "bold",
      hjust = .2,
      margin = margin(t = 10, l = 0, b = 20, r = 0)
    ),
    plot.background = element_rect(fill = "snow2", color = NA)
  ) +
  xlab("") +
  ylab("Groundhog Predictions Count") +
  ggtitle("Which States Has More Predictions Count?")
lollipop_groundhog

ggsave("lollipop_groundhog.png", lollipop_groundhog, device = ragg::agg_png)

