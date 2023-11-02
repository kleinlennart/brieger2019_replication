# Pilot Environmental Performance Index (EPI), 2006 Release (1994 – 2006) ----

# -> https://sedac.ciesin.columbia.edu/data/set/epi-pilot-environmental-performance-index-2006/data-download


epi_all <- readxl::read_excel(
  here::here("data", "EPI_2006", "2006-epi-all-countries.xlsx"),
  sheet = "EPI Data (238 countries)"
)

# -> missing values!

epi_only <- readxl::read_excel(
  here::here("data", "EPI_2006", "2006-epi-epi-only-countries.xlsx"),
  sheet = "EPI Data"
)

epi_only <- epi_only %>% select(Country, EPI)

countries <- dat$country %>%
  unique() %>%
  as.character()


epi_only %>% glimpse()
View(epi_only)
# https://fbellelli.github.io/countries/articles/dealing_with_names.html


setdiff(clean$country_chr, epi_only$Country)

# Different spelling:
# - Trinidad & Tobago
# - Viet Nam

# rest is missing in EPI data

merged <- countries::auto_merge(clean, epi_only,
  by = list("country" = c("country_chr", "Country")),
  country_to = "simple",
  merging_info = FALSE,
  verbose = TRUE,
  inner_join = TRUE,
  auto_melt = FALSE
)

clean %>%
  count(country_chr) %>%
  View()
merged %>%
  count(country) %>%
  View()


#
# Ukraine
# Switzerland
# South Korea
# Sweden
# Slovenia
# Autralia
# Chile
# Italy
# Inited States
# Morcoc
# Hunary
# Hgermamy
# Romania
# Brazil
# Psoain


# World Development Indicators 2005 ---------------------------------------

gdp <- read_csv("data/WorldBank/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6011335.csv", skip = 3)
gdp <- gdp %>% select(country_name = `Country Name`, GDP_2005 = `2005`)

merged_gdp <- countries::auto_merge(merged, gdp,
  by = list("COUNTRY" = c("country", "country_name")),
  country_to = "simple",
  merging_info = FALSE,
  verbose = TRUE,
  inner_join = TRUE,
  auto_melt = FALSE
)

merged_gdp %>% count(country_f)



# Averages ----------------------------------------------------------------

country_stats <- dat %>%
  group_by(country_chr) %>%
  summarise(
    postmaterialism_avg = mean(postmaterialism, na.rm = TRUE),
    sid_communal_avg = mean(sid_communal_num, na.rm = TRUE),
    sid_world_avg = mean(sid_world_num, na.rm = TRUE),
    sid_nation_avg = mean(sid_nation_num, na.rm = TRUE),
    env_concern_avg = mean(env_concern_avg, na.rm = TRUE),
  )
View(country_stats)

saveRDS(country_stats, "data/export/country_stats.rds")

