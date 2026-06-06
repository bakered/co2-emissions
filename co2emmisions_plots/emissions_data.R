library(tidyverse)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
library(zoo)
library(WDI)
library(ggrepel)

options(scipen=10000)
do_WDI = T

key <- read_csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"

if(do_WDI){
#### WDI from World Bank
indicators_list <- WDIsearch()
indicators <- c(
  "NY.GDP.MKTP.PP.KD",  #2021 dollars, i.e. real gdp, ppp
  "NY.GDP.PCAP.PP.KD", #2011 dollars per capita, ppp ## recalculate later!
  "SP.POP.TOTL", # total population
  #"EN.ATM.CO2E.KT", # co2 emissions kilotonnes.. fossil fuel + cement + gas flaring
  "EN.ATM.CO2E.PC" # co2 emissions metric tons per capita
  )

WDI_data <- WDI(indicator = indicators, start = 1960, end = format(Sys.Date(), "%Y"))
WDI_data = WDI_data %>% 
  rename(
    ISO3 = iso3c,
    gdp_WDI = NY.GDP.MKTP.PP.KD,
    gdp_per_capWDI = NY.GDP.PCAP.PP.KD,
    pop_WDI = SP.POP.TOTL
    #co2_WDI = EN.ATM.CO2E.KT,
    #co2_per_capWDI = EN.ATM.CO2E.PC
  ) %>% 
  select(ISO3, year, gdp_WDI, gdp_per_capWDI, pop_WDI) %>%  #, co2_WDI, co2_per_capWDI) %>% 
  filter(ISO3 != "") %>% 
  mutate(#co2_WDI = co2_WDI*1000, #convert to tons from kilotons
         gdp_per_capWDI = gdp_WDI / pop_WDI) #recalculated so that both are in 2021 dollars
}
#### MADDISON
maddison <- read_excel("mpd2023_web.xlsx", sheet = "Full data", col_names = TRUE) 
maddison = maddison %>% 
  rename(
    ISO3 = countrycode,
    gdp_per_cap_maddison = gdppc, #2011 international dollars ppp
    pop_maddison=pop,
  ) %>% 
  select(ISO3, year, pop_maddison, gdp_per_cap_maddison) %>% 
  filter(year>1820) %>% 
  mutate(pop_maddison = pop_maddison*1000) %>%  #change to individuals
  mutate(gdp_maddison = pop_maddison*gdp_per_cap_maddison)


#### GLOBAL CARBON PROJECT
co2_full <- read.csv("GCB2024v17_MtCO2_flat.csv")
co2_full = co2_full %>% 
  rename(
    c(country = Country, 
      ISO3 = ISO.3166.1.alpha.3, 
      year=Year, 
      co2_GCP=Total, 
      co2_per_cap_GCP = Per.Capita
    )
  ) %>% 
  select(ISO3, year, co2_GCP, co2_per_cap_GCP) %>% 
  mutate(co2_GCP = co2_GCP * 1000000) # to get tons from million tons

dataFull <- co2_full %>% 
  left_join(maddison, by=c("ISO3"="ISO3", "year"="year")) 
if(do_WDI){
dataFull = dataFull %>% 
  left_join(WDI_data, by=c("ISO3"="ISO3", "year"="year")) 
}
dataFull = dataFull %>% 
  left_join(key %>% select(UNCTAD_region, unctad_development_status, ISO3, ISO2, practical_readable), by=c("ISO3"="ISO3")) %>% 
  rename(region=UNCTAD_region,
         country=practical_readable) 
dataFull = dataFull %>% 
  filter(ISO3 != "WLD") %>% 
  group_by(ISO3) %>% 
  arrange(year) %>%  # Ensure data is ordered by year within each group
  mutate(
    # Calculate year-over-year growth rates for each variable
    gdp_WDI_growth = (gdp_WDI / lag(gdp_WDI)),
    gdp_per_capWDI_growth = (gdp_per_capWDI / lag(gdp_per_capWDI)),
    pop_WDI_growth = (pop_WDI / lag(pop_WDI))
  ) %>%
  # Extend Maddison data after 2022 using growth rates
  mutate(
    pop_maddison = if_else(year > 2022, lag(pop_maddison) * pop_WDI_growth, pop_maddison),
    gdp_per_cap_maddison = if_else(year > 2022, lag(gdp_per_cap_maddison) * gdp_per_capWDI_growth, gdp_per_cap_maddison),
    gdp_maddison = if_else(year > 2022, lag(gdp_maddison) * gdp_WDI_growth, gdp_maddison)
  ) %>% 
  ungroup()  # Remove grouping

colnames(dataFull) %>% dput()
ggplot(dataFull %>% filter(year==1990), 
       aes(y=gdp_per_capWDI, # gdp_WDI, #
           x=gdp_per_cap_maddison # gdp_maddison #
           )) +
  geom_point() +
  geom_abline() +
  geom_text_repel(aes(label = country), size = 3)  # Adding country labels with ggrepel


dataFull = dataFull %>% 
  mutate(
    region1=case_when(
      unctad_development_status == "developed" ~ "Developed",
      region == "Africa" ~ "Africa",
      region == "America" ~ "Latin America and the Caribbean",
      region == "Asia and Oceania" ~ "Developing Asia and Oceania",
      TRUE ~ "other"
    ),
    region2=case_when(
      ISO3 %in% LDCs ~ "LDCs",
      unctad_development_status == "developing" ~ "Developing",
      unctad_development_status == "developed" ~ "Developed",
      TRUE ~ "other"
    )
  ) %>% 
  filter(!ISO3 %in% c("ATA", "CXR", "PCZ", "KSV", "", "XIS", "XIA", "KNA", "TWN"))

################################################################################
# select GCP and Maddison
data = dataFull %>% 
  select(ISO3, year, co2_GCP, co2_per_cap_GCP, gdp_maddison, gdp_per_cap_maddison, pop_maddison, ISO2, country, region1, region2) %>%
  rename(
    co2 = co2_GCP, 
    co2_per_capita = co2_per_cap_GCP,
    gdp = gdp_maddison, 
    gdp_per_capita = gdp_per_cap_maddison, 
    pop = pop_maddison
  ) 

start_years <- data %>%
  filter(complete.cases(.)) %>% 
  group_by(ISO3) %>%
  summarize(first_year = min(year, na.rm = TRUE)) %>%
  ungroup()

# colnames(data)
start_years_continuous <- data %>%
  group_by(ISO3) %>%
  arrange(ISO3, year) %>%
  # Filter to keep only rows where one is an NA
  filter(is.na(co2) | is.na(pop) | is.na(gdp) | is.na(co2_per_capita) | is.na(gdp_per_capita)) %>%
  # Now find the first year for each country after which no missing data exists
  summarize(first_year_continuous = max(year)+1) %>%
  ungroup()

duplicates <- data %>%
  group_by(ISO3, year) %>%
  filter(n() > 1) %>%  # Keep only the groups with more than one row
  ungroup() %>% pull(ISO3) %>% 
  unique()
print(duplicates)

data <- data %>%
  # Join with the start_years_continuous dataframe
  left_join(start_years_continuous, by = "ISO3") %>%
  # Mutate the columns to NA if the year is less than the first_year_continuous
  mutate(
    co2 = if_else(year < first_year_continuous, NA_real_, co2),
    pop = if_else(year < first_year_continuous, NA_real_, pop),
    gdp = if_else(year < first_year_continuous, NA_real_, gdp),
    co2_per_capita = if_else(year < first_year_continuous, NA_real_, co2_per_capita),
    gdp_per_capita = if_else(year < first_year_continuous, NA_real_, gdp_per_capita)
  ) %>%
  # Optionally, remove the 'first_year_continuous' column if not needed anymore
  select(-first_year_continuous)

regions_data <- data %>%
  group_by(region1, year) %>%
  summarize(
    co2 = sum(co2, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    gdp = sum(gdp, na.rm = TRUE),
    .groups = 'drop'  # Ungroup the data after summarizing
  ) %>% 
  mutate(gdp_per_capita = gdp / pop, 
         co2_per_capita = co2 / pop) %>%
  group_by(region1) %>%
  arrange(region1, year) %>%
  mutate(accumulated_co2 = cumsum(co2)) %>%
  ungroup() %>% 
  mutate(
    co2 = rollmean(co2, 1, fill = "extend", align = "right"),
    gdp = rollmean(gdp, 1, fill = "extend", align = "right"),
    pop = rollmean(pop, 1, fill = "extend", align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 1, fill = "extend", align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 1, fill = "extend", align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 1, fill = "extend", align = "right")
  ) %>%
  ungroup() %>%
  mutate(across(c(gdp_per_capita, co2_per_capita), ~na_if(., Inf))) %>%
  mutate(
    co2 = ifelse(co2 < 0, 0, co2),
    pop = ifelse(pop < 0, 0, pop)
  )  %>% 
  mutate(gdp = gdp / 1e6, #turn gdp into millions
         co2 = co2 / 1000,
         accumulated_co2 = accumulated_co2 / 1000) # to convert to kilotons

is.na(regions_data$co2_per_capita) %>% table()

data = data %>%
  group_by(ISO3) %>%
  arrange(ISO3, year) %>%
  mutate(accumulated_co2 = cumsum(ifelse(is.na(co2), 0, co2)) + co2*0) %>%
  ungroup %>% 
  mutate(
    co2 = rollmean(co2, 1, fill = NA, align = "right"),
    gdp = rollmean(gdp, 1, fill = NA, align = "right"),
    pop = rollmean(pop, 1, fill = NA, align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 1, fill = NA, align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 1, fill = NA, align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 1, fill = NA, align = "right")
  ) %>%
  ungroup() %>% 
  mutate(gdp = gdp / 1e6, #convert gdp into millions
         co2 = co2 / 1000,
         accumulated_co2 = accumulated_co2 / 1000) # to convert to kilotons

data = data %>% filter(year>1820) 

write.csv(data, "dataCountries.csv", row.names = FALSE)
write.csv(regions_data, "dataRegions.csv", row.names = FALSE)
write.csv(data, "../co2emmisions_shinyapp/src_shiny_app/data/dataCountries.csv", row.names = FALSE)
write.csv(regions_data, "../co2emmisions_shinyapp/src_shiny_app/data/dataRegions.csv", row.names = FALSE)


static2022_countries = data %>% filter(year==2022, !is.na(gdp_per_capita), !is.na(co2_per_capita)) %>% select(ISO3, gdp_per_capita, co2_per_capita, co2, region1)
write.csv(static2022_countries, "static2022_countries.csv", row.names = FALSE)

static2022_regions = regions_data %>% filter(year==2022, !is.na(gdp_per_capita), !is.na(co2_per_capita)) %>% select(region1, gdp_per_capita, co2_per_capita, co2)
write.csv(static2022_regions, "static2022_regions.csv", row.names = FALSE)

static2023_countries = data %>% filter(year==2023, !is.na(gdp_per_capita), !is.na(co2_per_capita)) %>% select(ISO3, gdp_per_capita, co2_per_capita, co2, region1)
write.csv(static2023_countries, "static2023_countries.csv", row.names = FALSE)

static2023_regions = regions_data %>% filter(year==2023, !is.na(gdp_per_capita), !is.na(co2_per_capita)) %>% select(region1, gdp_per_capita, co2_per_capita, co2)
write.csv(static2023_regions, "static2023_regions.csv", row.names = FALSE)


if(F){
################################################################################
### repeat for WDI
#colnames(dataFull)
dataWDI = dataFull %>% 
  select(ISO3, year, co2_WDI, co2_per_capWDI, gdp_WDI, gdp_per_capWDI, pop_WDI, ISO2, country, region1, region2) %>% 
  rename(
    co2 = co2_WDI, 
    co2_per_capita = co2_per_capWDI, 
    gdp = gdp_WDI, 
    gdp_per_capita = gdp_per_capWDI, 
    pop = pop_WDI
  ) %>% 
  filter(year < 2021)

start_years <- dataWDI %>%
  filter(complete.cases(.)) %>% 
  group_by(ISO3) %>%
  summarize(first_year = min(year, na.rm = TRUE)) %>%
  ungroup()

start_years_continuous <- dataWDI %>%
  group_by(ISO3) %>%
  arrange(ISO3, year) %>%
  # Filter to keep only rows where one is an NA
  filter(is.na(co2) | is.na(pop) | is.na(gdp) | is.na(co2_per_capita) | is.na(gdp_per_capita)) %>%
  # Now find the first year for each country after which no missing data exists
  summarize(first_year_continuous = max(year)+1) %>%
  ungroup()

duplicates <- dataWDI %>%
  group_by(ISO3, year) %>%
  filter(n() > 1) %>%  # Keep only the groups with more than one row
  ungroup() %>% pull(ISO3) %>% 
  unique()
print(duplicates)

dataWDI <- dataWDI %>%
  # Join with the start_years_continuous dataframe
  left_join(start_years_continuous, by = "ISO3") %>%
  # Mutate the columns to NA if the year is less than the first_year_continuous
  mutate(
    co2 = if_else(year < first_year_continuous, NA_real_, co2),
    pop = if_else(year < first_year_continuous, NA_real_, pop),
    gdp = if_else(year < first_year_continuous, NA_real_, gdp),
    co2_per_capita = if_else(year < first_year_continuous, NA_real_, co2_per_capita),
    gdp_per_capita = if_else(year < first_year_continuous, NA_real_, gdp_per_capita)
  ) %>%
  # Optionally, remove the 'first_year_continuous' column if not needed anymore
  select(-first_year_continuous) 

regions_dataWDI <- dataWDI %>%
  group_by(region1, year) %>%
  summarize(
    co2 = sum(co2, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    gdp = sum(gdp, na.rm = TRUE),
    .groups = 'drop'  # Ungroup the data after summarizing
  ) %>% 
  mutate(gdp_per_capita = gdp / pop, 
         co2_per_capita = co2 / pop) %>% 
  group_by(region1) %>%
  arrange(region1, year) %>%
  mutate(accumulated_co2 = cumsum(ifelse(is.na(co2), 0, co2)) + co2*0) %>%
  ungroup() %>% 
  mutate(
    co2 = rollmean(co2, 1, fill = "extend", align = "right"),
    gdp = rollmean(gdp, 1, fill = "extend", align = "right"),
    pop = rollmean(pop, 1, fill = "extend", align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 1, fill = "extend", align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 1, fill = "extend", align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 1, fill = "extend", align = "right")
  ) %>%
  ungroup() %>%
  mutate(across(c(gdp_per_capita, co2_per_capita), ~na_if(., Inf))) %>%
  mutate(
    co2 = ifelse(co2 < 0, 0, co2),
    pop = ifelse(pop < 0, 0, pop)
  ) %>% 
  mutate(gdp = gdp / 1e6, #turn gdp into millions
         co2 = co2 / 1000,
         accumulated_co2 = accumulated_co2 / 1000) # to convert to kilotons

is.na(regions_dataWDI$co2_per_capita) %>% table()


dataWDI = dataWDI %>% 
  group_by(ISO3) %>%
  arrange(ISO3, year) %>%
  mutate(accumulated_co2 = cumsum(ifelse(is.na(co2), 0, co2)) + co2*0) %>%
  ungroup %>% 
  mutate(
    co2 = rollmean(co2, 1, fill = NA, align = "right"),
    gdp = rollmean(gdp, 1, fill = NA, align = "right"),
    pop = rollmean(pop, 1, fill = NA, align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 1, fill = NA, align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 1, fill = NA, align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 1, fill = NA, align = "right")
  ) %>%
  ungroup() %>% 
  mutate(gdp = gdp / 1e6, #turn gdp into millions
         co2 = co2 / 1000,
         accumulated_co2 = accumulated_co2 / 1000) # to convert to kilotons

dataWDI = dataWDI %>% 
  filter(year > 1989)

write.csv(dataWDI, "dataWDICountries.csv", row.names = FALSE)
write.csv(regions_dataWDI, "dataWDIRegions.csv", row.names = FALSE)

}


