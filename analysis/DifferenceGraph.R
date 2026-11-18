library(dplyr)
library(ggplot2)

dir.create("analysis", showWarnings = FALSE)

panel_2023 <- read.csv("app_data/panel_2023.csv", colClasses = c(fips = "character"))

summ <- panel_2023 %>%
  filter(state != "PR") %>%
  group_by(coal_status) %>%
  summarise(unemployment = median(unemployment_rate, na.rm = TRUE),
            pc_income = median(per_capita_income, na.rm = TRUE),
            hh_income = median(median_household_income_2022, na.rm = TRUE),
            gdp_pc = median(gdp_per_capita, na.rm = TRUE))

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

diffs

difference_graph <- ggplot(diffs, aes(indicator, diff, fill = coal_status)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("Active coal mine" = "#0072B2",
                               "Coal, no active mine" = "#D55E00")) +
  labs(title = "Coal counties vs. counties without coal, 2023",
       x = "", y = "", fill = "") +
  theme(legend.position = "bottom")

ggsave("analysis/difference_from_no_coal.png", difference_graph, width = 8, height = 5)

difference_graph