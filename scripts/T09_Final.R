library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(countrycode)
library(gt)
whr <- read_csv("WHR2024.csv")

#We will rename some of the columns to make the names more accesible:
whr_2 <- whr %>% 
  rename(
    Country= `Country name`,
    GDP= `Explained by: Log GDP per capita`,
    Social_support= `Explained by: Social support`,
    Life_expectancy= `Explained by: Healthy life expectancy`,
    Freedom= `Explained by: Freedom to make life choices`,
    Ladder_score= `Ladder score`,
    Generosity= `Explained by: Generosity`,
    Corruption= `Explained by: Perceptions of corruption`,
    Dystopia_res= `Dystopia + residual`
  )
View(whr_2)

whr_2$Continent <- countrycode(
  whr_2$Country,
  origin = "country.name",
  destination = "continent"
)

# Assign Kosovo to Europe manually
whr_2$Continent[whr_2$Country == "Kosovo"] <- "Europe"
# Replace remaining NA with "Other"
whr_2$Continent[is.na(whr_2$Continent)] <- "Other"



#1. Selecting the top 15 happiest countries
top15 <- whr_2 %>%
  arrange(desc(Ladder_score)) %>%
  slice(1:15)

#2. Reshaping data to long format
top15_long <- top15 %>%
  select(Country, Ladder_score, GDP, Social_support, Life_expectancy,
         Freedom, Generosity, Corruption, Dystopia_res) %>%
  pivot_longer(
    cols = c(GDP, Social_support, Life_expectancy, Freedom,
             Generosity, Corruption, Dystopia_res),
    names_to = "Factor",
    values_to = "Value"
  )

#3. Colours
factor_colors <- c(
  "GDP" = "#1f78b4",
  "Social_support" = "#33a02c",
  "Life_expectancy" = "#ff7f00",
  "Freedom" = "#6a3d9a",
  "Generosity" = "#b2df8a",
  "Corruption" = "#fb9a99",
  "Dystopia_res" = "#aaaaaa"
)

#4. Barplot
p1 <- ggplot(top15_long, aes(
  x = reorder(Country, Ladder_score),
  y = Value,
  fill = Factor
)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = factor_colors) +
  labs(
    title = "Top 15 Happiest Countries (2024)",
    subtitle = "Breakdown of the Ladder Score by Contributing Factors",
    caption = "Authors: Kirill Savin, Diego Montoya and Jorge Gamonal | Date: 2025-11-15",
    x = "",
    y = "Happiness Score Contribution",
    fill = "Factor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 20),
    axis.text.y = element_text(size = 12)
  )

p1
ggsave("Top15_Happiness_Factor_Decomposition.png",
       width = 12, height = 7, dpi = 320, bg = "white")




#5. Table plot to show each factor contribution
table_data <- top15 %>%
  select(Country, Ladder_score, GDP, Social_support, Life_expectancy,
         Freedom, Generosity, Corruption, Dystopia_res) %>%
  arrange(desc(Ladder_score)) %>%
  mutate(across(-Country, ~round(.x, 3)))

table_gt <- table_data %>%
  gt() %>%
  tab_header(
    title = "Factor Contributions for the Top 15 Happiest Countries (2024)",
    subtitle = "All components of the Ladder Score"
  ) %>%
  fmt_number(
    columns = -Country,
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Authors: Kirill Savin, Diego Montoya and Jorge Gamonal| Date: 2025-11-16"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = 20,
    heading.subtitle.font.size = 14
  )



table_gt
gtsave(table_gt, "Top15_Happiness_Factors_Table.png")


#6. Graph p2 - Average Factor Contributions in World Happiness
p2 <- whr_2 %>%
  select(GDP, Social_support, Life_expectancy, Freedom, Generosity, Corruption, Dystopia_res) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Factor",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = reorder(Factor, Value), y = Value, fill = Factor)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = factor_colors) +
  labs(
    title = "Average Factor Contributions in World Happiness (2024)",
    subtitle = "Breakdown of Average Ladder Score by Contributing Factors",
    caption = "Authors: Kirill Savin, Diego Montoya and Jorge Gamonal | Date: 2025-11-15",
    x = "Factor",
    y = "Average Happiness Score Contribution"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p2
ggsave("World_Happiness_Factor_Averages.png",
       width = 10, height = 6, dpi = 320, bg = "white")

