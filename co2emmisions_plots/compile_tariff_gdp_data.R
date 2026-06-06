
library(tidyverse)
library(WDI)
library(dplyr)

key <- read.csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"


# Define the indicators
indicators <- c(
  "NY.GDP.MKTP.CD",    # GDP (Current US$)
  "GC.TAX.IMPT.ZS",    # Customs and Other Import Duties (% of Tax Revenue)
  "GC.TAX.TOTL.GD.ZS"  # Tax Revenue (% of GDP)
)

# Fetch the data
raw_data <- WDI(indicator = indicators, country = "all", start = 2015, end = 2025)

raw_data$iso3c[raw_data$iso3c=="EUU"] = "EUN"

gdp_data = raw_data %>% 
  rename("value" = NY.GDP.MKTP.CD) %>% 
  filter(year>2015 & !is.na(value))  %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>% 
  select(country, iso3c, value, year) %>% 
  rename("gdp_current_usd" = value,
         "gdp_current_usd_year" = "year")
tariff_data = raw_data %>% 
  rename("value" = GC.TAX.IMPT.ZS) %>% 
  filter(year>2015 & !is.na(value))  %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>% 
  select(country, iso3c, value, year) %>% 
  rename("tariff_as_share_tax" = value,
         "tariff_as_share_tax_year" = "year")
tax_data = raw_data %>% 
  rename("value" = GC.TAX.TOTL.GD.ZS) %>% 
  filter(year>2015 & !is.na(value))  %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>% 
  select(country, iso3c, value, year) %>% 
  rename("tax_as_share_gdp" = value,
         "tax_as_share_gdp_year" = "year")

data = gdp_data %>% 
  full_join(tariff_data, by=c("country", "iso3c")) %>% 
  full_join(tax_data, by=c("country", "iso3c")) %>% 
  filter(iso3c != "" & iso3c != "WLD") %>% 
  left_join(key %>% select(ISO3, UNCTAD, datawrapper_maps, unctad_development_status, datawrapper_flags, SIDS_UNCTAD, LLDC, LDC), by=c("iso3c"="ISO3")) %>% 
  left_join(lat_lon, by=c("datawrapper_flags"="DW")) %>% 
  select(-datawrapper_flags, -LON) %>% 
  rename("LON"=LON.1,
         "ISO3"=iso3c) %>% 
  filter(!is.na(unctad_development_status) | ISO3=="EUN") %>% 
  mutate(datawrapper_maps = str_pad(datawrapper_maps, width = 3, side = "left", pad = "0"))


write.csv(data, "tariff_gdp_data.csv", row.names = FALSE)
