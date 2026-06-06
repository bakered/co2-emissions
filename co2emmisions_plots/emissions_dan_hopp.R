library(tidyverse)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
options(scipen=10000)

# carbon data from: https://globalcarbonatlas.org/emissions/carbon-emissions/, type=territorial, unit=mtco2

key <- read_csv("multi_country_key.csv")

open_7z <- function (series_name) {
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

# population
pop <- open_7z("US.PopTotal")
pop <- pop %>% 
  rename(population=Absolute.value.in.thousands, country=Economy.Label, year=Year) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States", country)) %>% 
  select(country, year, population) %>% 
  left_join(key %>% select(ISO3, UNCTAD), by=c("country"="UNCTAD")) %>% 
  tibble() %>% 
  filter(!is.na(ISO3), !is.na(population)) %>% 
  select(ISO3, year, population) %>% 
  mutate(population = population * 1000, year = as.numeric(year)) # population is originally in thousands

# GDP
gdp <- open_7z("US.GDPTotal")
gdp <- gdp %>% 
  rename(gdp=US..at.constant.prices..2015..in.millions, country=Economy.Label, year=Year) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States", country)) %>% 
  select(country, year, gdp) %>% 
  left_join(key %>% select(ISO3, UNCTAD), by=c("country"="UNCTAD")) %>% 
  tibble() %>% 
  filter(!is.na(ISO3), !is.na(gdp)) %>% 
  select(ISO3, year, gdp) %>% 
  mutate(gdp = gdp * 1e6, year = as.numeric(year)) # gdp is originally in millions usd

# emissions, in millions tons of CO2
co2 <- read_excel("export_emissions.xlsx")
colnames(co2) <- data.frame(co2)[1,]
colnames(co2)[1] <- "year"
co2 <- co2 %>% 
  slice(2:nrow(.))
co2 <- co2 %>% 
  pivot_longer(-year, "country") %>% 
  rename(co2 = value) %>% 
  left_join(key %>% select("ISO3", "carbon_atlas"), by=c("country"="carbon_atlas")) %>% 
  select(ISO3, year, co2) %>% 
  mutate(co2 = as.numeric(co2) * 1e6, year = as.numeric(year)) %>%  # conver to tons
  filter(!is.na(co2))

data <- co2 %>% 
  left_join(pop, by=c("ISO3"="ISO3", "year"="year")) %>% 
  left_join(gdp, by=c("ISO3"="ISO3", "year"="year")) %>% 
  filter(!is.na(co2), !is.na(population), !is.na(gdp)) %>% 
  mutate(gdp_per_capita = gdp / population, co2_per_capita = co2 / population) %>% 
  #pivot_longer(cols=c("co2", "population", "gdp", "gdp_per_capita", "gdp_per_capita", "co2_per_capita")) %>% 
  tibble() %>% 
  left_join(key %>% select(UNCTAD_region, ISO3), by=c("ISO3"="ISO3")) %>% 
  rename(region=UNCTAD_region) %>% 
  mutate(gdp = gdp / 1e6)

# plot
p <- data %>% 
  ggplot() +
  aes(x = gdp, y = co2_per_capita, color=region, text=
        paste(
          "Country:", ISO3,
          "<br>GDP (m USD):", round(gdp, 0),
          "<br>Population (th):", round(population / 1e3, 0),
          "<br>GDP per capita", round(gdp_per_capita, 0),
          "<br>CO2 emissions (m tons)", round(co2, 0),
          "<br>CO2 emissions per capita", round(co2_per_capita, 2)
        )) +
  geom_point(aes(size=co2, frame=year, ids=ISO3, alpha=0.75)) +
  scale_x_log10() +
  scale_size_continuous(range = c(0.01, 50)) +
  labs(x="GDP in millions", y="Tons of CO2 emissions per capita", alpha="", color="", size="") +
  scale_y_continuous(limits = c(0, 65))

  
p <- ggplotly(p, tooltip="text") %>% 
  layout(hoverlabel = list(align = "left")) 
p

htmlwidgets::saveWidget(as_widget(p), "emissions.html")
