library(dplyr)
library(leaflet)
library(sf)
library(tigris)
library(htmlwidgets)

options(tigris_use_cache = TRUE)
dir.create("analysis", showWarnings = FALSE)

panel_2023 <- read.csv("app_data/panel_2023.csv", colClasses = c(fips = "character"))

shapes <- counties(cb = TRUE, resolution = "20m", year = 2023) %>%
  filter(as.integer(STATEFP) < 60) %>%
  st_transform(4326)

money <- function(x) ifelse(is.na(x), "No data", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))
pct <- function(x) ifelse(is.na(x), "No data", paste0(round(x, 1), "%"))
num <- function(x) ifelse(is.na(x), "No data", formatC(x, format = "f", digits = 0, big.mark = ","))

map_data <- shapes %>%
  left_join(panel_2023, by = c("GEOID" = "fips")) %>%
  mutate(county_name = ifelse(is.na(county_name), NAMELSAD, county_name),
         state = ifelse(is.na(state), STUSPS, state),
         coal_status = ifelse(is.na(coal_status), "No data", coal_status),
         popup = paste0(
           "<b>", county_name, ", ", state, "</b><br>",
           "Coal status: ", coal_status, "<br><br>",
           "Unemployment rate: ", pct(unemployment_rate), "<br>",
           "Per-capita income: ", money(per_capita_income), "<br>",
           "Median household income (2022): ", money(median_household_income_2022), "<br>",
           "GDP per person: ", money(gdp_per_capita), "<br>",
           "Mining share of GDP: ", pct(mining_share_gdp * 100), "<br>",
           "Population: ", num(population), "<br><br>",
           "Active coal mines (2024): ", num(coal_mines_2024), "<br>",
           "Coal production (2024, thousand tons): ", num(coal_production_2024)))

pal <- colorFactor(c("#0072B2", "#D55E00", "grey70", "white"),
                   levels = c("Active coal mine", "Coal, no active mine", "No coal", "No data"))

county_map <- leaflet(map_data) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(coal_status),
              fillOpacity = 0.7,
              color = "white",
              weight = 0.3,
              label = ~paste0(county_name, ", ", state),
              popup = ~popup,
              highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~coal_status, title = "Coal status")

saveWidget(county_map, "analysis/county_map_2023.html")

county_map
