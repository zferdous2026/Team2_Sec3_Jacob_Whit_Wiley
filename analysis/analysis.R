library(dplyr)
library(ggplot2)

dir.create("analysis", showWarnings = FALSE)

panel_2023 <- read.csv("app_data/panel_2023.csv", colClasses = c(fips = "character"))

panel_2023 <- panel_2023 %>%
  filter(state != "PR") %>%
  mutate(coal_status = factor(coal_status,
                              levels = c("Active coal mine", "Coal, no active mine", "No coal")))

g1 <- panel_2023 %>%
  group_by(coal_status) %>%
  summarise(med_unemployment = median(unemployment_rate, na.rm = TRUE)) %>%
  ggplot(aes(coal_status, med_unemployment)) +
  geom_col(fill = "steelblue") +
  labs(title = "Unemployment rate by coal status, 2023 (median county)",
       x = "", y = "Unemployment rate (%)")

g2 <- panel_2023 %>%
  group_by(coal_status) %>%
  summarise(med_income = median(median_household_income_2022, na.rm = TRUE)) %>%
  ggplot(aes(coal_status, med_income)) +
  geom_col(fill = "steelblue") +
  labs(title = "Median household income by coal status (median county)",
       x = "", y = "Median household income, 2022 ($)")

g3 <- panel_2023 %>%
  arrange(desc(coal_status)) %>%
  ggplot(aes(unemployment_rate, median_household_income_2022, color = coal_status)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Active coal mine" = "firebrick",
                                "Coal, no active mine" = "orange",
                                "No coal" = "grey70")) +
  labs(title = "Unemployment vs. household income by coal status, 2023",
       x = "Unemployment rate, 2023 (%)",
       y = "Median household income, 2022 ($)",
       color = "")

summ <- panel_2023 %>%
  group_by(coal_status) %>%
  summarise(unemployment = median(unemployment_rate, na.rm = TRUE),
            pc_income = median(per_capita_income, na.rm = TRUE),
            hh_income = median(median_household_income_2022, na.rm = TRUE),
            gdp_pc = median(gdp_per_capita, na.rm = TRUE),
            mining = median(mining_share_gdp, na.rm = TRUE) * 100)

all_indicators <- rbind(
  data.frame(coal_status = summ$coal_status, indicator = "Unemployment rate (%)", value = summ$unemployment),
  data.frame(coal_status = summ$coal_status, indicator = "Per-capita income ($)", value = summ$pc_income),
  data.frame(coal_status = summ$coal_status, indicator = "Median household income ($)", value = summ$hh_income),
  data.frame(coal_status = summ$coal_status, indicator = "GDP per person ($)", value = summ$gdp_pc),
  data.frame(coal_status = summ$coal_status, indicator = "Mining share of GDP (%)", value = summ$mining)
)

g4 <- ggplot(all_indicators, aes(coal_status, value, fill = coal_status)) +
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

g5 <- ggplot(panel_2023, aes(coal_status, unemployment_rate, fill = coal_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Active coal mine" = "firebrick",
                               "Coal, no active mine" = "orange",
                               "No coal" = "grey70")) +
  labs(title = "Spread of unemployment rates by coal status, 2023",
       x = "", y = "Unemployment rate (%)") +
  theme(legend.position = "none")

g6 <- ggplot(panel_2023, aes(coal_status, median_household_income_2022, fill = coal_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Active coal mine" = "firebrick",
                               "Coal, no active mine" = "orange",
                               "No coal" = "grey70")) +
  labs(title = "Spread of household income by coal status",
       x = "", y = "Median household income, 2022 ($)") +
  theme(legend.position = "none")

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

g7 <- ggplot(diffs, aes(indicator, diff, fill = coal_status)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("Active coal mine" = "firebrick",
                               "Coal, no active mine" = "orange")) +
  labs(title = "How coal counties compare to counties without coal, 2023",
       subtitle = "% difference from the median county with no coal (0 = same as no-coal counties)",
       x = "", y = "% difference from no-coal counties", fill = "") +
  theme(legend.position = "bottom")

ggsave("analysis/all_indicators_by_coal_status.png", g4, width = 12, height = 5)
ggsave("analysis/difference_from_no_coal.png", g7, width = 8, height = 5)
ggsave("analysis/unemployment_boxplot.png", g5, width = 7, height = 5)
ggsave("analysis/income_boxplot.png", g6, width = 7, height = 5)
ggsave("analysis/unemployment_by_coal_status.png", g1, width = 7, height = 5)
ggsave("analysis/income_by_coal_status.png", g2, width = 7, height = 5)
ggsave("analysis/unemployment_vs_income.png", g3, width = 8, height = 5)

g1
g2
g3
g4
g5
g6
g7