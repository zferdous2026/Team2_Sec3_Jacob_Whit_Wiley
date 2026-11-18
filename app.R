
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# ---------- DATA ----------
panel_2023 <- read.csv("app_data/panel_2023.csv", colClasses = c(fips = "character"))

# ---------- MAP PREP (runs once when the app starts) ----------
shapes <- counties(cb = TRUE, resolution = "20m", year = 2023) %>%
  filter(as.integer(STATEFP) < 60) %>%
  st_transform(4326)

money <- function(x) ifelse(is.na(x), "No data", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))
pct   <- function(x) ifelse(is.na(x), "No data", paste0(round(x, 1), "%"))
num   <- function(x) ifelse(is.na(x), "No data", formatC(x, format = "f", digits = 0, big.mark = ","))

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

# ---------- GRAPH 1 PREP ----------
# Uses its own copy (panel_graph) so the map's panel_2023 is not changed
panel_graph <- panel_2023 %>%
  filter(state != "PR") %>%
  mutate(coal_status = factor(coal_status,
                              levels = c("Active coal mine", "Coal, no active mine", "No coal")))

summ <- panel_graph %>%
  group_by(coal_status) %>%
  summarise(unemployment = median(unemployment_rate, na.rm = TRUE),
            pc_income = median(per_capita_income, na.rm = TRUE),
            hh_income = median(median_household_income_2022, na.rm = TRUE),
            gdp_pc = median(gdp_per_capita, na.rm = TRUE))

all_indicators <- rbind(
  data.frame(coal_status = summ$coal_status, indicator = "Unemployment rate (%)", value = summ$unemployment),
  data.frame(coal_status = summ$coal_status, indicator = "Per-capita income ($)", value = summ$pc_income),
  data.frame(coal_status = summ$coal_status, indicator = "Median household income ($)", value = summ$hh_income),
  data.frame(coal_status = summ$coal_status, indicator = "GDP per person ($)", value = summ$gdp_pc)
)

# ---------- GRAPH 2 PREP (reuses summ from Graph 1) ----------
base <- summ %>% filter(coal_status == "No coal")
coal_groups <- summ %>% filter(coal_status != "No coal")

pct_diff <- function(x, b) (x / b - 1) * 100

diffs <- rbind(
  data.frame(coal_status = coal_groups$coal_status, indicator = "Unemployment rate",
             diff = pct_diff(coal_groups$unemployment, base$unemployment)),
  data.frame(coal_status = coal_groups$coal_status, indicator = "Per-capita income",
             diff = pct_diff(coal_groups$pc_income, base$pc_income)),
  data.frame(coal_status = coal_groups$coal_status, indicator = "Median household income",
             diff = pct_diff(coal_groups$hh_income, base$hh_income)),
  data.frame(coal_status = coal_groups$coal_status, indicator = "GDP per person",
             diff = pct_diff(coal_groups$gdp_pc, base$gdp_pc))
)

# ---------- UI ----------
ui <- navbarPage(
  title = "Mining and Markets",
  
  tabPanel("About",
           h3("About This Project"),
           p(strong("Project question:"), "Write the question your team is investigating here."),
           p(strong("Data source:"), "Describe where the data comes from (for example, Census Bureau, BEA, EIA/MSHA) and the year."),
           p(strong("Team members:"), "List all team members' names here.")
  ),
  
  tabPanel("Interactive Map",
           h3("Coal Status and Economic Conditions by County (2023)"),
           p("Description: what the map shows, the main pattern you see, and why it matters.
       Click a county to see its details."),
           leafletOutput("county_map", height = 650)
  ),
  
  tabPanel("Graph 1",
           h3("Economic Indicators by Coal Status, 2023"),
           p("Description: this graph compares the median county in each coal group on
       unemployment, per-capita income, household income, and GDP per person.
       Add the main pattern (with numbers) and why it matters."),
           plotOutput("indicators_graph", height = "450px")
  ),
  
  tabPanel("Graph 2",
           h3("Coal Counties vs. Counties Without Coal, 2023"),
           p("Description: this graph shows how far each coal group's median county is
       above or below the median county with no coal, as a percent difference.
       Bars to the right of zero are higher than no-coal counties; bars to the
       left are lower. Add the main pattern (with numbers) and why it matters."),
           plotOutput("difference_graph", height = "450px")
  ),
  
  tabPanel("Data",
           h3("Dataset (first 20 rows)"),
           tableOutput("data_table")
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  output$county_map <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(coal_status),
                  fillOpacity = 0.7,
                  color = "white",
                  weight = 0.3,
                  label = ~paste0(county_name, ", ", state),
                  popup = ~popup,
                  highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>%
      addLegend("bottomright", pal = pal, values = ~coal_status, title = "Coal status")
  })
  
  output$indicators_graph <- renderPlot({
    ggplot(all_indicators, aes(coal_status, value, fill = coal_status)) +
      geom_col() +
      facet_wrap(~indicator, scales = "free_y", nrow = 1) +
      scale_fill_manual(values = c("Active coal mine" = "#0072B2",
                                   "Coal, no active mine" = "#D55E00",
                                   "No coal" = "grey70")) +
      labs(title = "Economic indicators by coal status, 2023 (median county)",
           x = "", y = "", fill = "") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "bottom")
  })
  
  output$difference_graph <- renderPlot({
    ggplot(diffs, aes(indicator, diff, fill = coal_status)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0) +
      coord_flip() +
      scale_fill_manual(values = c("Active coal mine" = "#0072B2",
                                   "Coal, no active mine" = "#D55E00")) +
      labs(title = "Coal counties vs. counties without coal, 2023",
           x = "", y = "", fill = "") +
      theme(legend.position = "bottom")
  })
  
  output$data_table <- renderTable({
    head(panel_2023, 20)
  })
  
}

shinyApp(ui = ui, server = server)