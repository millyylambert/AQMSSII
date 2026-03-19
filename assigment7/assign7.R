setwd("~/Desktop/AQMSSII/assignment7")
library(sf)
library(spData)
library(tidyverse)
library(ggplot2)
data(world)

### PART 1: EXPLORING SPATIAL DATA

## 1.1 Inspecting an sf object 

# a) 
class(world)
names(world)
nrow(world)

# An sf object is a dataframe with a geometry column that stores spatial shapes. 

# b) 
st_crs(world)

# That dataset uses ESPG:4326 (WSG - World Geodetic System 1984). WGS84 is the global
# standard coordinate system. Coordinates are expressed in decimal degrees of longitude
# (east-west) and latitude (north-south), making it suitable for global datasets where 
# a common datum is needed across all regions. 

# c) 
unique(st_geometry_type(world))

# The geometry tuype is a multipolygon - a collection of one or more polygons treated 
# as a single geographic feature. Countries require multiple polygons when their 
# territory is not a single contiguous land mass e.g. France's overseas territories. 

# d) 
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

plot(world["gdpPercap"], main = "GDP per capita by country")

# The map shows that North America, Australia/NZ and Western and Northern Europe
# have the highest GDP per capitas. Sub-saharan Africa and South/Southeast Asia 
# have the lowest. East Asia and parts of South America occupy the middle. 

## 1.2 Attribute operations 

# a) 
africa = filter(world, continent == "Africa")
nrow(africa)

plot(africa["gdpPercap"], main = "GDP per capita -- Africa")

# The dataset contains 51 African countries. The UN recognizes 54 African states, 
# so this count is slightly below expectations and likely reflects missing data or 
# the exclusion of very small terrritores from teh spData world polygon dataset. 

# b) 

world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

plot(gdp_by_continent)

print(st_drop_geometry(gdp_by_continent))

# When summarise() is called on a grouped sf object, it unions the geometries within
# each group and retains the resulting geometry column. To obtain a plain data frame
# without spatial information, use st_drop_geometry() before or after the summary 
# step. 

# c) 
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))

# Equatorial Guinea, Gabon, Libya, Botswana, Algeria.


## 1.3 Simple visualization with ggplot2

# a) 
p <- ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country") +
  coord_sf(crs = 3035)

ggsave("world_gdp.pdf", plot = p, width = 10, height = 5)

# The geographic pattern mirrors what the base-R map showed. Western Europe, North
# America and Oceania stand otu as the wealthiest regions. East Asia shows a gradiant from 
# high (Japan and South Korea) to middle (China). Sub-saharan Africa and South Asia 
# concentrate the lowest values. 

# b) 
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa")

ggsave("africa_gdp.pdf", width = 7, height = 6)

# Within Africa, there is substantial variation. A cluster of relatively wealthier 
# countries appears in North Africa and Southern Africa. Central and West Africa,
# with the exception of oil-rich Equatorial Guinea and Gabon, display persistent 
# structural poverty.

# c) 
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")

ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

# White country borders improves readability. 


### PART 2: POINT DATA AND SPATIAL JOINS

events <- read_csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")

## 2.1 Converting tabular data to sf

# a) 
events_sf <- st_as_sf(events,
         coords = c("longitude", "latitude"),
         crs = 4326)

class(events_sf)
st_crs(events_sf)

# st_as_sf() converts a regular dataframe into a spatial object by turning coordinate 
# columns into geometry. The coords argument specifies which columns contain the 
# longitude and latitude values that define each point's location. The argument 
# CRS = 4362 assigns the coordinate reference system WGS84, which is the standard 
# global system used for GDP coordinates. 

# b) 
nrow(events_sf)
table(events_sf$event_type)

# 6835 events in the dataset, state-based events are the most common. 

# c) 
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_sf(data = events_sf, aes(color = event_type), size = 1, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Conflict Events Map",
       color = "Event Type") 

# The conflict events are clustered in Africa because these regions experience ongoing 
# instability. This clustering reflects the localised nature of conflict dynamics, 
# where violence persists in specific areas rather than being globally uniform. 

## 2.2 Spatial join: events to countries 

# a) 
st_crs(events_sf)
st_crs(world)

events_joined <- st_join(events_sf, world)
nrow(events_joined) == nrow(events_sf)

# st_join() matches each event-point to the polygon (country) that contains it. 
# Checking that both objects have the same CRS is essential because the spatial 
# systems rely on different coordinate systems; if the CRS differs, ocations will 
# not align correctly, leading to incorrect matches. 

# b) 
sum(is.na(events_joined$name_long))
mean(is.na(events_joined$name_long))

# The mean is 0.02, meaning 2% of events do not match any country polygon. This 
# could be because these events took place at sea or exactly on borders. 

# c) 
summary_table <- events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(
    events = n(),
    fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  arrange(desc(events))

summary_table %>%
  st_drop_geometry() %>%
  head(10) 

# One would expect the countries with the highest number of events and fatalities  
# to be in regions experiencing ongoing conflict, such as sub-Saharan Africa. The results 
# are consistent with these known patterns of contemporary armed conflict. That said, 
# there are notably missing countries like Ukraine, Palestine, Sudan, Myanmar and others 
# which are among the most deadly conflicts globally. 

## 2.3 Choropleth of conflict intensity 

# a) 
event_counts_df <- summary_table %>%
  st_drop_geometry()

world_events <- world %>%
  left_join(event_counts_df, by = "name_long")
world_events <- world_events %>%
  mutate(
    events = replace_na(events, 0),
    fatalities = replace_na(fatalities, 0)
  )

nrow(world_events) == nrow(world)

# The event counts match meanign that the join preserved the full set of country 
# polygons. 

# b) 

ggplot(world_events) +
  geom_sf(aes(fill = events), color = "white") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  labs(
    title = "Conflict Event Counts by Country",
    fill = "Number of Events"
  )

ggsave("choropleth_conflict_events.png", width = 10, height = 6)

# The geographic pattern matches the event-level map from question 2.1c, with most 
# of the data points clustered in Africa. 

# c) 
ggplot(world_events) +
  geom_sf(aes(fill = log1p(events)), color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Log(events+1)"
  ) +
  theme_minimal() +
  labs(title = "Log-Transformed Conflict Event Counts by Country")

ggsave("conflict_log_map.pdf", width = 10, height = 6)

# The log transformation is useful because conflict events are highly skewed with 
# a small number of countries experiencing very high levels of violence while
# most countries have few or no events. The log function compresses the scale
# and reduces the dominance of extreme values. It shows more gradiation between 
# the countries.

## 2.5 Discussion

# When events fall exactly on borders, they are often misallocated (or not allocated)
# to the correct polygons. Similarly, small coordinate imprecicion can place points 
# outside of a country's boundary, leading to missing matches. One way to address 
# this would be to apply a small buffer around country polygons or use nearest-neighbour
# matching, which assings each point to its nearest polygon even if it falls slightly 
# outside its boundary. 

# st_join() performs a spatial join based on geographic relationships between 
# geometries, such as whether a point lies within a polygon. It uses the spatial 
# coordinates and shapes of the data to determine matches. In contrast, left_join()
# is an attribute-based join that matches rows using common key variables, such as 
# country names or IDs, without considering spatial information. st_join() is 
# preferred when matching data based on location (e.g. assigning events to countries)
# while left_join() is used when combining datasets that already share a common 
# identifier. 
