library(dplyr)

find_file <- function(name) {
  path <- list.files(".", pattern = name, recursive = TRUE, full.names = TRUE)
  if (length(path) == 0) stop(paste("Can't find", name, "in", getwd()))
  path[1]
}

unemp <- read.csv(find_file("Unemployment2023.csv"))
income <- read.csv(find_file("CAINC1__ALL_AREAS_1969_2024.csv"), fileEncoding = "latin1")
gdp <- read.csv(find_file("GDP.csv"), fileEncoding = "latin1")
coal <- read.csv(find_file("USCoal.csv"))
mines <- read.csv(find_file("eia_coal_2024.csv"), colClasses = c(fips = "character"))

unemp <- unemp %>%
  mutate(fips = sprintf("%05d", FIPS_Code)) %>%
  filter(substr(fips, 3, 5) != "000")

income <- income %>%
  mutate(fips = gsub('[" ]', "", GeoFIPS)) %>%
  filter(nchar(fips) == 5, substr(fips, 3, 5) != "000")

gdp <- gdp %>%
  mutate(fips = gsub('[" ]', "", GeoFIPS)) %>%
  filter(nchar(fips) == 5, substr(fips, 3, 5) != "000")

get_unemp <- function(attribute, name) {
  rows <- unemp %>% filter(Attribute == attribute)
  out <- data.frame(fips = rows$fips, value = rows$Value)
  names(out)[2] <- name
  out
}

get_bea <- function(data, line, yr, name) {
  rows <- data %>% filter(LineCode == line)
  out <- data.frame(bea_fips = rows$fips,
                    value = suppressWarnings(as.numeric(rows[[paste0("X", yr)]])))
  names(out)[2] <- name
  out
}

va <- data.frame(
  bea_fips = rep(c("51901", "51903", "51907", "51911", "51913", "51918", "51919", "51921",
                   "51923", "51929", "51931", "51933", "51939", "51941", "51942", "51944",
                   "51945", "51947", "51949", "51951", "51953", "51955", "51958"),
                 c(2, 2, 3, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 2)),
  fips = c("51003", "51540", "51005", "51580", "51015", "51790", "51820", "51031", "51680",
           "51035", "51640", "51053", "51570", "51730", "51059", "51600", "51610", "51069",
           "51840", "51081", "51595", "51089", "51690", "51095", "51830", "51121", "51750",
           "51143", "51590", "51149", "51670", "51153", "51683", "51685", "51161", "51775",
           "51163", "51530", "51678", "51165", "51660", "51175", "51620", "51177", "51630",
           "51191", "51520", "51195", "51720", "51199", "51735")
)

county_list <- unemp %>%
  group_by(fips) %>%
  summarise(state = State[1],
            county_name = gsub(", [A-Z]{2}$", "", Area_Name[1])) %>%
  left_join(va, by = "fips") %>%
  mutate(bea_combined = !is.na(bea_fips),
         bea_fips = ifelse(is.na(bea_fips), fips, bea_fips))

clean <- function(x) {
  x <- toupper(x)
  x <- gsub("SAINT", "ST", x)
  x <- gsub("COUNTY|PARISH|BOROUGH|CENSUS AREA|MUNICIPALITY|/.*", "", x)
  gsub("[^A-Z]", "", x)
}

lookup <- data.frame(State = county_list$state,
                     key = clean(county_list$county_name),
                     fips = county_list$fips)

fixes <- data.frame(
  State = c("AK", "AK", "AK", "AK", "AK", "IN", "CA", "NM"),
  County = c("KENAI-COOK INLET DIV", "MATANUSKA-SUSITNA DIV", "SOUTHEAST FAIRBANKS DIV",
             "YUKON-KOYUKUK DIV", "BARROW-NORTH SLOPE DIV", "GREEN", "MONTERY", "SANTE FE"),
  fix_fips = c("02122", "02170", "02240", "02290", "02185", "18055", "06053", "35049")
)

coal_summary <- coal %>%
  mutate(key = clean(County)) %>%
  left_join(lookup, by = c("State", "key")) %>%
  left_join(fixes, by = c("State", "County")) %>%
  mutate(fips = ifelse(is.na(fips), fix_fips, fips)) %>%
  group_by(fips) %>%
  summarise(coal_records = n(),
            coal_tonnage_total = sum(Tonnage),
            coal_tonnage_meas_ind = sum(Tonnage[Reliability != "Inferred" & Reliability != "Unclassified"]),
            coal_main_rank = Rank[which.max(Tonnage)],
            coal_province = Coal.Province[1],
            coal_region = Coal.Region[1],
            coal_first_assessment_yr = min(Year),
            coal_last_assessment_yr = max(Year))

counties <- county_list %>%
  left_join(get_unemp("Median_Household_Income_2022", "median_household_income_2022"), by = "fips") %>%
  left_join(get_unemp("Rural_Urban_Continuum_Code_2023", "rural_urban_continuum_code_2023"), by = "fips") %>%
  left_join(get_unemp("Metro_2023", "metro_2023"), by = "fips") %>%
  left_join(coal_summary, by = "fips") %>%
  left_join(mines[, c("fips", "coal_mines_2024", "coal_mines_underground_2024",
                      "coal_mines_surface_2024", "coal_production_2024")], by = "fips") %>%
  mutate(coal_production_2024 = ifelse(is.na(coal_mines_2024), 0, coal_production_2024),
         coal_mines_underground_2024 = ifelse(is.na(coal_mines_2024), 0, coal_mines_underground_2024),
         coal_mines_surface_2024 = ifelse(is.na(coal_mines_2024), 0, coal_mines_surface_2024),
         coal_mines_2024 = ifelse(is.na(coal_mines_2024), 0, coal_mines_2024),
         has_coal_mine = coal_mines_2024 > 0,
         has_coal = !is.na(coal_records),
         coal_records = ifelse(is.na(coal_records), 0, coal_records),
         coal_tonnage_total = ifelse(is.na(coal_tonnage_total), 0, coal_tonnage_total),
         coal_tonnage_meas_ind = ifelse(is.na(coal_tonnage_meas_ind), 0, coal_tonnage_meas_ind))

make_year <- function(yr) {
  bea <- get_bea(income, 1, yr, "personal_income_k") %>%
    full_join(get_bea(income, 2, yr, "population"), by = "bea_fips") %>%
    full_join(get_bea(income, 3, yr, "per_capita_income"), by = "bea_fips") %>%
    full_join(get_bea(gdp, 1, yr, "gdp_total_k"), by = "bea_fips") %>%
    full_join(get_bea(gdp, 2, yr, "gdp_private_k"), by = "bea_fips") %>%
    full_join(get_bea(gdp, 6, yr, "gdp_mining_k"), by = "bea_fips") %>%
    full_join(get_bea(gdp, 12, yr, "gdp_manufacturing_k"), by = "bea_fips") %>%
    full_join(get_bea(gdp, 83, yr, "gdp_government_k"), by = "bea_fips")
  
  data.frame(fips = county_list$fips, bea_fips = county_list$bea_fips) %>%
    left_join(bea, by = "bea_fips") %>%
    left_join(get_unemp(paste0("Civilian_labor_force_", yr), "civilian_labor_force"), by = "fips") %>%
    left_join(get_unemp(paste0("Employed_", yr), "employed"), by = "fips") %>%
    left_join(get_unemp(paste0("Unemployed_", yr), "unemployed"), by = "fips") %>%
    left_join(get_unemp(paste0("Unemployment_rate_", yr), "unemployment_rate"), by = "fips") %>%
    mutate(year = yr,
           mining_share_gdp = gdp_mining_k / gdp_total_k,
           gdp_per_capita = gdp_total_k * 1000 / population)
}

panel <- data.frame()
for (yr in 2001:2023) {
  panel <- rbind(panel, make_year(yr))
}

panel <- panel %>%
  mutate(bea_fips = NULL) %>%
  left_join(counties, by = "fips") %>%
  arrange(fips, year)

panel <- panel %>%
  mutate(coal_status = ifelse(has_coal_mine, "Active coal mine",
                              ifelse(has_coal, "Coal, no active mine", "No coal")))

keep <- c("fips", "state", "county_name", "year", "coal_status", "has_coal", "has_coal_mine",
          "coal_mines_2024", "coal_production_2024", "coal_tonnage_total",
          "population", "unemployment_rate", "per_capita_income",
          "median_household_income_2022", "gdp_per_capita", "mining_share_gdp")

panel_small <- panel[, keep]

panel_2023 <- panel_small %>% filter(year == 2023)

dir.create("app_data", showWarnings = FALSE)
saveRDS(counties, "app_data/counties.rds")
saveRDS(panel, "app_data/panel.rds")
write.csv(counties, "app_data/counties.csv", row.names = FALSE)
write.csv(panel_small, "app_data/panel.csv", row.names = FALSE)
write.csv(panel_2023, "app_data/panel_2023.csv", row.names = FALSE)