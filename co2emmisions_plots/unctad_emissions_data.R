library(tidyverse)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
library(zoo)
library(WDI)

options(scipen=10000)

key <- read_csv("multi_country_key.csv")

# function to download unctad data
open_7z <- function (series_name) { #series_name = "US.PopTotal"
  version <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/bulkfile/") %>% 
    GET() %>% 
    .$content %>% 
    rawToChar() %>% 
    fromJSON() %>% 
    .$version
  file_id <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/${version}/bulkfiles/en") %>% 
    GET() %>% 
    .$content %>% 
    rawToChar() %>% 
    fromJSON() %>% 
    .$fileId
  url <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/${version}/bulkfile/${file_id}/en")
  
  file_name_like <- str_replace(series_name, "\\." , "_")
  
  tmps <- tempfile(fileext = ".7z")
  download.file(url, tmps, quiet = T, mode = "wb")
  tmps_path <- str_split(tmps, "/")
  tmps_file <- str_split(tmps, "/")[[1]][length(tmps_path[[1]])]
  tmps_path <- paste0(paste(tmps_path[[1]][1:length(tmps_path[[1]])-1], collapse="/"), "/")
  print(tmps_path)
  # unzip the file
  system(str_interp('cd ${tmps_path} && /opt/homebrew/bin/7z x ${tmps_file} -aoa > nul && rm nul'))
  for (csv_file in list.files(tmps_path)) {
    if (grepl(file_name_like, csv_file)) {
      rawdata <- read.csv(paste0(tmps_path, csv_file), header=T, stringsAsFactors=F)
      system(str_interp('rm ${paste0(tmps_path, csv_file)}'))
    }
  }
  unlink(tmps)
  return (rawdata)
}

# unctad population
unctad_pop <- open_7z("US.PopTotal")
unctad_pop <- unctad_pop %>% 
  rename(population=Absolute.value.in.thousands, country=Economy.Label, year=Year) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States", country)) %>% 
  select(country, year, population) %>% 
  left_join(key %>% select(ISO3, UNCTAD), by=c("country"="UNCTAD")) %>% 
  tibble() %>% 
  filter(!is.na(ISO3), !is.na(population)) %>% 
  select(ISO3, year, population) %>% 
  mutate(population = population * 1000, year = as.numeric(year)) # population is originally in thousands

# unctad gdp
unctad_gdp <- open_7z("US.GDPTotal")
unctad_gdp <- unctad_gdp %>% 
  rename(gdp=US..at.constant.prices..2015..in.millions, country=Economy.Label, year=Year) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States", country)) %>% 
  select(country, year, gdp) %>% 
  left_join(key %>% select(ISO3, UNCTAD), by=c("country"="UNCTAD")) %>% 
  tibble() %>% 
  filter(!is.na(ISO3), !is.na(gdp)) %>% 
  select(ISO3, year, gdp) %>% 
  mutate(gdp = gdp * 1e6, year = as.numeric(year)) %>% # gdp is originally in millions usd
  distinct(ISO3, year, .keep_all = TRUE)  

# emissions, in millions tons of CO2, from global carbon project website
# carbon data from: https://globalcarbonatlas.org/emissions/carbon-emissions/, type=territorial, unit=mtco2
co2 <- read_excel("export_emissions.xlsx")
colnames(co2) <- data.frame(co2)[1,]
colnames(co2)[1] <- "year"
co2 <- co2 %>% 
  slice(2:nrow(.))
co2 <- co2 %>% 
  pivot_longer(cols=-year, names_to = "country", values_to = "co2") %>% 
  left_join(key %>% select("ISO3", "ISO2", "carbon_atlas"), by=c("country"="carbon_atlas")) %>% 
  select(ISO3, ISO2, year, co2) %>% 
  mutate(co2 = as.numeric(co2) * 1e6, year = as.numeric(year)) %>%  # convert to tons
  filter(!is.na(co2))

data <- co2 %>% 
  left_join(unctad_pop, by=c("ISO3"="ISO3", "year"="year")) %>% 
  left_join(unctad_gdp, by=c("ISO3"="ISO3", "year"="year")) %>% 
  filter(!is.na(co2), !is.na(population), !is.na(gdp)) %>% 
  #pivot_longer(cols=c("co2", "population", "gdp", "gdp_per_capita", "gdp_per_capita", "co2_per_capita")) %>% 
  tibble() %>% 
  left_join(key %>% select(UNCTAD_region, unctad_development_status, ISO3), by=c("ISO3"="ISO3")) %>% 
  rename(region=UNCTAD_region) 

start_years <- data %>%
  group_by(ISO3) %>%
  summarize(first_year = min(year, na.rm = TRUE)) %>%
  ungroup()

data = data %>% 
  mutate(region2 = case_when(
    (unctad_development_status == "developed" & ISO3 %in% (start_years%>%filter(first_year<1991)%>%pull(ISO3)))  ~ "developed (excluding eastern block)",
    region == "Africa" ~ "Africa",
    region == "America" ~ "Developing America",
    region == "Asia and Oceania" ~ "Developing Asia"
  )) 

regions_data <- data %>%
  group_by(region2, year) %>%
  summarize(
    co2 = sum(co2, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    gdp = sum(gdp, na.rm = TRUE),
    .groups = 'drop'  # Ungroup the data after summarizing
  ) %>% 
  mutate(gdp_per_capita = gdp / population, co2_per_capita = co2 / population) %>% 
  mutate(gdp = gdp / 1e6) %>% 
  group_by(region2) %>%
  arrange(region2, year) %>%
  mutate(accumulated_co2 = cumsum(co2)) %>%
  mutate(
    co2 = rollmean(co2, 3, fill = NA, align = "right"),
    gdp = rollmean(gdp, 3, fill = NA, align = "right"),
    population = rollmean(population, 3, fill = NA, align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 3, fill = NA, align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 3, fill = NA, align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 3, fill = NA, align = "right")
  ) %>%
  filter(!is.na(co2) & !is.na(gdp) & !is.na(population) &
           !is.na(gdp_per_capita) & !is.na(co2_per_capita) &
           !is.na(accumulated_co2)) %>%
  ungroup()


data = data %>% 
  mutate(gdp_per_capita = gdp / population, co2_per_capita = co2 / population) %>% 
  mutate(gdp = gdp / 1e6) %>% 
  group_by(ISO3) %>%
  arrange(ISO3, year) %>%
  mutate(accumulated_co2 = cumsum(co2)) %>%
  mutate(
    co2 = rollmean(co2, 3, fill = NA, align = "right"),
    gdp = rollmean(gdp, 3, fill = NA, align = "right"),
    population = rollmean(population, 3, fill = NA, align = "right"),
    gdp_per_capita = rollmean(gdp_per_capita, 3, fill = NA, align = "right"),
    co2_per_capita = rollmean(co2_per_capita, 3, fill = NA, align = "right"),
    accumulated_co2 = rollmean(accumulated_co2, 3, fill = NA, align = "right")
  ) %>%
  filter(!is.na(co2) & !is.na(gdp) & !is.na(population) &
           !is.na(gdp_per_capita) & !is.na(co2_per_capita) &
           !is.na(accumulated_co2)) %>% 
  ungroup()


write.csv(data, "dataCountriesUnctad.csv", row.names = FALSE)
write.csv(regions_data, "dataRegionsUnctad.csv", row.names = FALSE)