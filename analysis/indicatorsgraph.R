library(dplyr)
library(ggplot2)

dir.create("analysis", showWarnings = FALSE)

panel_2023 <- read.csv("app_data/panel_2023.csv", colClasses = c(fips = "character"))

panel_2023 <- panel_2023 %>%
  filter(state != "PR") %>%
  mutate(coal_status = factor(coal_status,
                              levels = c("Active coal mine", "Coal, no active mine", "No coal")))

summ <- panel_2023 %>%
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

indicators_graph <- ggplot(all_indicators, aes(coal_status, value, fill = coal_status)) +
  geom_col() +
  facet_wrap(~indicator, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Active coal mine" = "firebrick",
                               "Coal, no active mine" = "orange",
                               "No coal" = "grey70")) +
  labs(title = "Economic indicators by coal status, 2023 (median county)",
       x = "", y = "", fill = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")

ggsave("analysis/indicators_by_coal_status.png", indicators_graph, width = 10, height = 5)

indicators_graph