library(tidyverse)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
library(DatawRappr)

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
  left_join(key %>% select(ISO3, UNCTAD, unctad_development_status), by=c("country"="UNCTAD")) %>% 
  tibble()

ISO3_map = c(
  "Cote d'Ivoire" = "CIV",
  "Curacao" = "CUW",
  "Dem. People's Rep. of Korea" = "PRK",
  "Dem. Rep. of the Congo" = "",
  "Ethiopia (...1991)" = "",
  "Indonesia (...2002)" = "",
  "Netherlands (Kingdom of the)"= "",
  "Republic of Korea" = "",
  "Republic of Moldova"= "",
  "Sudan (...2011)"= "",
  "Switzerland"= "",
  "Tanganyika"   = "",
  "Turkiye"= "",
  "United Republic of Tanzania"= "",
  "United States"   = "",
  "Yemen, Arab Republic"    = "",
  "Yemen, Democratic"= ""
)

gdp_countries <- gdp %>% 
  filter(!is.na(ISO3), !is.na(gdp)) %>% 
  select(ISO3, year, gdp, unctad_development_status) %>% 
  mutate(gdp = gdp * 1e6, year = as.numeric(year)) # gdp is originally in millions usd

gdp_countries = gdp_countries %>% 
  group_by(ISO3) %>% 
  arrange(year) %>%  # Ensure data is ordered by year within each group
  mutate(
    # Calculate year-over-year growth rates for each variable
    growth_rate = (gdp / lag(gdp)-1) * 100
  ) 

gdp_developed = gdp %>% 
  select(-ISO3, -unctad_development_status) %>% 
  filter(country %in% c("World", "Developed economies", "Developing economies")) %>%
  group_by(country) %>% 
  arrange(year) %>%  # Ensure data is ordered by year within each group
  mutate(
    # Calculate year-over-year growth rates for each variable
    growth_rate = (gdp / lag(gdp)-1) *100
  ) %>% 
  ungroup
  
new_rows <- tibble(
  country = c("World", "World", "Developing economies", "Developed economies", 
              "Developing economies", "Developed economies"),
  year = c(2024, 2025, 2024, 2024, 2025, 2025),
  gdp = NA,  # We will not be providing GDP values, just the growth rates
  growth_rate = c(2.7, 2.7, 4.1, 1.8, 4.2, 1.7)  # Provided growth rates
)
gdp_developed <- bind_rows(gdp_developed, new_rows)


gdp_developed %>% 
  ggplot(aes(x=year, y=growth_rate, colour=country)) +
  geom_line()

library(scales)  # For percentage and better axis formatting

gdp_developed %>% 
  ggplot(aes(x = year, y = growth_rate, colour = country)) +
  geom_line(linewidth = 1.2) +  # Make lines thicker for better readability
  scale_color_brewer(palette = "Set1") +  # Use a colorblind-friendly palette
  scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
  labs(
    title = "GDP Growth Rate Over Time",
    x = "Year",
    y = "Growth Rate (%)",
    colour = "Country"
  ) +
  theme_minimal(base_size = 14) +  # Set a minimal theme with a larger font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    legend.position = "bottom",  # Place the legend at the bottom for better use of space
    legend.title = element_text(face = "bold")  # Bold legend title
  )

# Calculate the averages for each period
avg_1970_2007 <- gdp_developed %>% 
  filter(year >= 1970 & year <= 2007 & country == "World") %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE)) %>%
  pull(avg_growth_rate)


avg_2000_2007 <- gdp_developed %>% 
  filter(year >= 2000 & year <= 2007 & country == "World") %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE)) %>%
  pull(avg_growth_rate)

avg_2011_2019 <- gdp_developed %>% 
  filter(year >= 2011 & year <= 2019 & country == "World") %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE)) %>%
  pull(avg_growth_rate)

avg_2022_2025 <- gdp_developed %>% 
  filter(year >= 2022 & year <= 2025 & country == "World") %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE)) %>%
  pull(avg_growth_rate)

avg_2008_2025 <- gdp_developed %>% 
  filter(year >= 2008 & year <= 2025 & country == "World") %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE)) %>%
  pull(avg_growth_rate)


plot = gdp_developed %>%
  filter(country=="World") %>% 
  #filter(year >= 2000) %>%  # Filter data from 2004 onward
  ggplot(aes(x = factor(year), y = growth_rate, fill = country)) +  # Make year a factor for discrete bars
  geom_col(position = "dodge", width = 0.7) +  # Use dodged bars for country comparison
  scale_fill_brewer(palette = "Set1") +  # Color palette for country
  scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
  labs(
    title = "GDP Growth Rate",
    x = "Year",
    y = "Growth Rate (%)",
    fill = "Country"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with increased font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add space to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add space to y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Tilt x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase y-axis label size
    legend.position = "bottom",  # Place legend at the bottom
    legend.title = element_text(face = "bold")  # Bold legend title
  ) 

plot = plot +
  # Add dotted horizontal lines for the average of each period
  geom_segment(aes(x = 1, xend = 8, y = avg_2000_2007, yend = avg_2000_2007), 
               linetype = "dotted", color = "black", size = 1) +
  geom_segment(aes(x = 12, xend = 20, y = avg_2011_2019, yend = avg_2011_2019), 
               linetype = "dotted", color = "black", size = 1) +
  geom_segment(aes(x = 23, xend = 26, y = avg_2022_2025, yend = avg_2022_2025), 
               linetype = "dotted", color = "black", size = 1)
 
plot + annotate("text", x = 1, y = avg_2000_2007, label = paste("Avg 2000-2007:", round(avg_2000_2007, 2)), hjust = 0, vjust = -0.5, color = "black") +
   annotate("text", x = 12, y = avg_2011_2019, label = paste("Avg 2011-2019:", round(avg_2012_2019, 2)), hjust = 0, vjust = -0.5, color = "black") +
  annotate("text", x = 23, y = avg_2022_2025, label = paste("Avg 2022-2025:", round(avg_2022_2025, 2)), hjust = 0, vjust = -0.5, color = "black")

if(F){
# datawrpaper
datawrapper_auth(api_key = Sys.getenv("DATAWRAPPER_TOKEN"), overwrite = TRUE)
# make sure the key is working as expected
dw_test_key()
 
plot_dw = gdp_developed %>% filter(country=="World" & year > 1999) %>% select(year,growth_rate) 

 dw_create_chart(        
   type = 'column-chart'
 )
 dw_data_to_chart(    
   plot_dw,    
   chart_id = "uaNW3"  
 )
 dw_export_chart("uaNW3")
}
