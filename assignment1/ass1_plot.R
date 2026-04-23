getwd()
list.files("data")
library(ggplot2)
gapminder <- read.csv("data/gapminder.csv")
head(gapminder)
str(gapminder)
countries <- c("Albania", "Finland", "Morocco", "United Kingdom")
df <- gapminder[gapminder$country %in% countries, ]

ggplot(df, aes(x = year, y = pop, color = country)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Population",title = "Population over time") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggsave("assignment1/ass1_plot.png", width = 7, height = 5)

library(scales)
