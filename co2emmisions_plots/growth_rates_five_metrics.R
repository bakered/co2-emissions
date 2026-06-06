library(tidyverse)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
library(DatawRappr)
library(scales)  # For percentage and better axis formatting
library(WDI)

options(scipen=10000)

# carbon data from: https://globalcarbonatlas.org/emissions/carbon-emissions/, type=territorial, unit=mtco2

key <- read_csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"


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
  filter(country %in% c("World", "Developing economies", "Developed economies")) %>% 
  select(country, year, population) %>% 
  mutate(population = population * 1000, year = as.numeric(year)) # population is originally in thousands

# GDP
GDP_raw <- open_7z("US.GDPTotal")
GDP_raw <- GDP_raw %>% 
  rename(GDP_2015=US..at.constant.prices..2015..in.millions, 
         country=Economy.Label, 
         year=Year,
         GDP=US..at.current.prices.in.millions) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States", country)) %>% 
  select(country, year, GDP_2015, GDP, Economy) %>% 
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), by=c("Economy"="UNCTAD_code")) %>% 
  tibble()

GDP_raw %>% filter(year==2023) %>%  filter(!is.na(unctad_development_status)) %>% View()

GDP_raw %>% filter(year==2023) %>%  filter(!is.na(unctad_development_status)) %>% filter(!is.na(GDP_2015)) %>% 
  group_by(unctad_development_status) %>% 
  summarise(GDP_2015 = sum(GDP_2015)) %>% 
  ungroup

GDP_raw %>% filter(year==2023) %>% 
  filter(country %in% c("Developing economies", "Developed economies"))

CPI = GDP_raw %>% 
  filter(country=="World") %>% 
  mutate(cpi = GDP*100/GDP_2015) %>% 
  select(year, cpi)

CPI2 <- read.csv("~/UN_projects/c02emmisions/gdp_forecasts/US_CommodityPriceIndices_A.csv")
CPI2 = CPI2 %>% filter(Commodity.Label == "All groups")

plot(CPI$year, CPI$cpi)

FDI <- read.csv("~/UN_projects/c02emmisions/gdp_forecasts/US_FdiFlowsStock.csv") %>% 
  filter(Economy.Label %in% c("World", "Developing economies", "Developed economies") & Flow.Label == "Flow" & Direction.Label == "Inward") %>% 
  rename(country = Economy.Label,
         year = Year,
         FDI = US..at.current.prices.in.millions) %>% 
  select(country, year, FDI) %>% 
  left_join(CPI, by = "year") %>% 
  mutate(
    FDI_2015 = FDI * (100 / cpi)
  ) %>% 
  arrange(country, year) %>%  
  group_by(country) %>% 
  mutate(
    FDI_gr = (FDI / lag(FDI) - 1) * 100, # Calculate growth rate as a percentage
    FDI_2015_gr = (FDI_2015 / lag(FDI_2015) - 1) * 100 # Calculate growth rate as a percentage
  ) %>% 
  ungroup()
  


GFCF_raw = WDI(indicator = "NE.GDI.FTOT.CD", country = "all")  #NE.GDI.FTOT.ZS is as share of gdp
GFCF =  GFCF_raw %>% 
  rename( 
    ISO3 = iso3c,
    GFCF = NE.GDI.FTOT.CD
  ) %>% 
  filter(ISO3 != "") %>% 
  left_join(key %>% select(UNCTAD_region, unctad_development_status, ISO3, ISO2, practical_readable), by=c("ISO3"="ISO3")) %>% 
  rename(region=UNCTAD_region) %>% 
  mutate(unctad_development_status = ifelse(country == "World", "World", unctad_development_status)) %>% 
  filter(!is.na(unctad_development_status)) %>% 
  group_by(year, unctad_development_status) %>% 
  summarise(GFCF=sum(GFCF, na.rm = T)) %>%    # xxxxxxxxxxx TREATING NA DATA AS ZEROS xxxxxxxxxxxxx
  select(unctad_development_status, year, GFCF) %>% 
  rename(country = unctad_development_status) %>% 
  ungroup() %>% 
  mutate(GFCF = ifelse(GFCF==0, NA, GFCF)) %>% 
  left_join(CPI, by = "year") %>% 
  mutate(
    GFCF_2015 = GFCF * (100 / cpi)
  ) %>% 
  arrange(country, year) %>%  
  group_by(country) %>% 
  mutate(
    GFCF_gr = (GFCF / lag(GFCF) - 1) * 100, # Calculate growth rate as a percentage
    GFCF_2015_gr = (GFCF_2015 / lag(GFCF_2015) - 1) * 100 # Calculate growth rate as a percentage
  ) %>% 
  ungroup()
  
trade <- read.csv("~/UN_projects/highcharter/show_highcharts/bubble_map/US_TradeMerchTotal.csv") %>% 
  filter(Economy.Label %in% c("World", "Developing economies", "Developed economies")) %>% 
  rename(country = Economy.Label,
         year = Year) %>% 
  group_by(year, country) %>% 
  summarise(trade = mean(US..at.current.prices.in.millions)) %>% # mean of imports and exports
  ungroup() %>% 
  left_join(CPI, by = "year") %>% 
  mutate(
    trade_2015 = trade * (100 / cpi)
  ) %>% 
  arrange(country, year) %>%  
  group_by(country) %>% 
  mutate(
    trade_gr = (trade / lag(trade) - 1) * 100, # Calculate growth rate as a percentage
    trade_2015_gr = (trade_2015 / lag(trade_2015) - 1) * 100 # Calculate growth rate as a percentage
  ) %>% 
  ungroup()
  

debt_raw = WDI(indicator = "GC.DOD.TOTL.GD.ZS", country = "all")  #GC.DOD.TOTL.GD.ZS is as share of gdp
debt  =  debt_raw %>% 
  rename( 
    ISO3 = iso3c,
    debt = GC.DOD.TOTL.GD.ZS 
  ) %>% 
  filter(ISO3 != "") %>% 
  left_join(key %>% select(UNCTAD_region, unctad_development_status, ISO3, ISO2, practical_readable), by=c("ISO3"="ISO3")) %>% 
  rename(region=UNCTAD_region) %>% 
  mutate(unctad_development_status = ifelse(country == "World", "World", unctad_development_status)) %>% 
  filter(!is.na(unctad_development_status)) %>% 
  group_by(year, unctad_development_status) %>% 
  summarise(debt=sum(debt, na.rm = T)) %>%    # xxxxxxxxxxx TREATING NA DATA AS ZEROS xxxxxxxxxxxxx
  select(unctad_development_status, year, debt) %>% 
  rename(country = unctad_development_status) %>% 
  ungroup() %>% 
  mutate(debt = ifelse(debt ==0, NA, debt )) %>% 
  arrange(country, year) %>%  
  group_by(country) %>% 
  mutate(
    debt_gr = (debt / lag(debt ) - 1) * 100 # Calculate growth rate as a percentage
  ) %>% 
  ungroup()


# debt IMF from world of debt
debt_raw2 = read.csv("~/UN_projects/c02emmisions/gdp_forecasts/WorldOfDebt_data.csv")
debt2  =  debt_raw2 %>% 
  filter(Indicator == "Public debt in US$ billions") %>% 
  rename(debt2 = Value,
         year = Year) %>% 
  group_by(Development.status, year) %>% 
  summarise(debt2 = sum(debt2)) %>% 
  ungroup() %>% 
  rename(country = Development.status) 
debt2$year = as.integer(debt2$year)
debt2 = bind_rows(debt2, debt2 %>% group_by(year) %>% summarise(debt2 = sum(debt2)) %>% mutate(country="World")) %>% 
  left_join(CPI, by = "year") %>% 
  mutate(
    debt2_2015 = debt2 * (100 / cpi)
  ) %>% 
  arrange(country, year) %>%  
  group_by(country) %>% 
  mutate(
    debt2_gr = (debt2 / lag(debt2 ) - 1) * 100, # Calculate growth rate as a percentage
    debt2_2015_gr = (debt2_2015 / lag(debt2_2015 ) - 1) * 100 # Calculate growth rate as a percentage
  ) %>% 
  ungroup() %>% 
  filter(!is.na(country))

####################### GDP
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

gdp_countries <- GDP_raw %>% 
  filter(!is.na(ISO3), !is.na(GDP_2015)) %>% 
  select(ISO3, year, GDP_2015, unctad_development_status) %>% 
  mutate(GDP_2015 = GDP_2015 * 1e6, year = as.numeric(year)) # gdp is originally in millions usd

gdp_countries = gdp_countries %>% 
  group_by(ISO3) %>% 
  arrange(year) %>%  # Ensure data is ordered by year within each group
  mutate(
    # Calculate year-over-year growth rates for each variable
    growth_rate = (GDP_2015 / lag(GDP_2015)-1) * 100
  ) 

GDP = GDP_raw %>% 
  select(-ISO3, -unctad_development_status) %>% 
  filter(country %in% c("World", "Developed economies", "Developing economies")) %>%
  group_by(country) %>% 
  arrange(year) %>%  # Ensure data is ordered by year within each group
  mutate(
    # Calculate year-over-year growth rates for each variable
    GDP_gr = (GDP / lag(GDP)-1) *100,
    GDP_2015_gr = (GDP_2015 / lag(GDP_2015)-1) *100
  ) %>% 
  ungroup()

# manually add new rows
new_rows <- tibble(
  country = c("World", "World", "Developing economies", "Developed economies", 
              "Developing economies", "Developed economies"),
  year = c(2024, 2025, 2024, 2024, 2025, 2025),
  GDP_2015 = NA,  #not providing GDP values, just the growth rates
  GDP = NA,
  GDP_gr = NA,
  GDP_2015_gr = c(2.7, 2.7, 4.1, 1.8, 4.2, 1.7)  # Provided growth rates
)
GDP <- bind_rows(GDP, new_rows)

line_growth_rate <- function(data, growth_rate="GDP_2015_gr", year = "year", country = "country", title = "Growth Rate Over Time") { #data=debt2
  # Convert growth_rate column to symbol for dynamic column selection
  growth_rate <- rlang::sym(growth_rate)
  year <- rlang::sym(year)
  country <- rlang::sym(country)
  
  ggplot(data, aes(x = !!year, y = !!growth_rate, colour = !!country)) +
    geom_line(linewidth = 1.2) +  # Make lines thicker for better readability
    scale_color_brewer(palette = "Set1") +  # Use a colorblind-friendly palette
    scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
    labs(
      title = title,
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
}

bar_growth_rate <- function(data, growth_rate="GDP_2015_gr", country = "country", year = "year", title = "GDP Growth Rate") {
  ggplot(data %>%
           filter(!!rlang::sym(country) == "World"), 
         aes(x = factor(!!rlang::sym(year)), y = !!rlang::sym(growth_rate), fill = !!rlang::sym(country))) + 
    geom_col(position = "dodge", width = 0.7) +  # Use dodged bars for country comparison
    scale_fill_brewer(palette = "Set1") +  # Color palette for country
    scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
    labs(
      title = title,
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
}

save_folder <- "gdp_forecasts/growth_dfs"
# Save each data frame as an R object in the growth_dfs folder
save(GDP, file = file.path(save_folder, "GDP.RData"))
save(FDI, file = file.path(save_folder, "FDI.RData"))
save(GFCF, file = file.path(save_folder, "GFCF.RData"))
save(trade, file = file.path(save_folder, "trade.RData"))
save(debt, file = file.path(save_folder, "debt.RData"))
save(debt2, file = file.path(save_folder, "debt2.RData"))

GDP %>% filter(year>1999 & year<2023) %>%line_growth_rate(growth_rate = 'GDP_2015_gr', title = "GDP Growth Rate")
GDP %>% filter(year>1999 & year<2023) %>%bar_growth_rate(growth_rate = "GDP_2015_gr", title = "GDP Growth Rate")

FDI %>% filter(year>1999 & year<2023) %>%line_growth_rate(growth_rate = "FDI_2015_gr", title = "FDI Growth Rate")
FDI %>% filter(year>1999 & year<2023) %>%bar_growth_rate(growth_rate = "FDI_2015_gr", title = "FDI Growth Rate")

GFCF %>% filter(year>1999 & year<2023) %>% line_growth_rate(growth_rate = "GFCF_2015_gr", title = "GFCF Growth Rate")
GFCF %>% filter(year>1999 & year<2023) %>% bar_growth_rate(growth_rate = "GFCF_2015_gr", title = "GFCF Growth Rate")

trade %>% filter(year>1999 & year<2023) %>% line_growth_rate(growth_rate = "trade_2015_gr", title = "Trade Growth Rate")
trade %>% filter(year>1999 & year<2023) %>% bar_growth_rate(growth_rate = "trade_2015_gr", title = "Trade Growth Rate")

debt2 %>% filter(year>1999 & year<2023) %>% line_growth_rate(growth_rate = "debt2_2015_gr", title = "Debt Growth Rate")
debt2 %>% filter(year>1999 & year<2023) %>% bar_growth_rate(growth_rate = "debt2_2015_gr", title = "Debt Growth Rate")


GFCF <- GFCF %>%
  mutate(country = recode(country,
                          "World" = "World",
                          "developing" = "Developing economies",
                          "developed" = "Developed economies")) 

debt <- debt %>%
  mutate(country = recode(country,
                          "World" = "World",
                          "developing" = "Developing economies",
                          "developed" = "Developed economies")) 

combined <- GDP  %>%
  left_join(FDI %>% select(-cpi), by = c("country", "year")) %>%
  left_join(GFCF%>% select(-cpi) , by = c("country", "year")) %>%
  left_join(trade%>% select(-cpi) , by = c("country", "year")) %>%
  left_join(debt, by = c("country", "year")) %>%
  left_join(debt2%>% select(-cpi) , by = c("country", "year")) %>% 
  left_join(pop, by = c("country", "year"))

add_per_capita_and_growth <- function(data, vars) {
  data %>%
    group_by(country) %>%  # Group by country
    arrange(country, year) %>%  # Ensure data is arranged by country and year
    mutate(
      across(
        all_of(vars),  # Apply the transformation for each variable in vars
        ~ .x / population,  # Create the per-capita column
        .names = "{.col}_pc"
      )) %>% 
    mutate(
      across(
        all_of(vars),  # Apply the transformation for growth rate
        ~ ((.x / population) - lag(.x / population))* 100 / (lag(.x / population)) , # Calculate year-to-year growth rate
        .names = "{.col}_pc_gr"
      )
    ) %>%
    ungroup()  # Ungroup after processing
}



# colnames(combined) %>% dput()
vars <- c("GDP_2015", "GDP", "FDI", "FDI_2015", "GFCF", "GFCF_2015", 
          "trade", "trade_2015", "debt", "debt2", "debt2_2015")  # List of variables
combined <- add_per_capita_and_growth(combined, vars)

save(combined, file = file.path(save_folder, "combined.RData"))
save(combined, file = file.path("gdp_forecasts/growth_charts1", "combined.RData"))


data_long <- combined %>%
  filter(year > 1999) %>% 
  filter(country == "Developing economies") %>% 
  gather(key = "growth_rate_type", value = "growth_rate_value", all_of(c("GDP_gr", "FDI_gr", "trade_gr")))

d_ing = ggplot(data_long, aes(x = year, y = growth_rate_value, colour = growth_rate_type)) +
  geom_line(linewidth = 1.2) +  # Make lines thicker for better readability
  scale_color_brewer(palette = "Set1") +  # Color palette
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentages
  labs(
    title = "Growth Rate of Developing Economies",
    x = "Year",
    y = "Growth Rate (%)",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with larger font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    legend.position = "bottom",  # Place the legend at the bottom for better use of space
    legend.title = element_text(face = "bold")  # Bold legend title
  )


annualized_growth_rate <- function(data, start_year, end_year, growth_col = "GDP_2015") { #data =combined %>%filter(country=="World")
  # Ensure the data is filtered to the relevant years
  data_filtered <- data %>% 
    filter(year %in% c(start_year, end_year))
  
  if (nrow(data_filtered) != 2) {
    stop("Dataframe has too many or too few rows for those years.")
  }
  
  # Extract GDP values for the start and end years
  gdp_start <- data_filtered %>% filter(year == start_year) %>% pull(!!rlang::sym(growth_col))
  gdp_end <- data_filtered %>% filter(year == end_year) %>% pull(!!rlang::sym(growth_col))
  
  # Calculate the number of years
  num_years <- end_year - start_year
  
  # Calculate the annualized growth rate
  growth_rate <- (gdp_end / gdp_start)^(1 / num_years) - 1
  
  return(growth_rate*100)
}
annualized_growth_rate(data=combined %>%filter(country=="World"), start_year = 2000, end_year = 2010, growth_col="FDI_2015")
annualized_growth_rate(data=combined %>%filter(country=="World"), start_year = 2010, end_year = 2020, growth_col="FDI_2015")
annualized_growth_rate(data=combined %>%filter(country=="World"), start_year = 2020, end_year = 2022, growth_col="FDI_2015")



### data for doughnut charts

doughnuts = GDP_raw %>% 
  filter(year==2023 & country %in% c("Developing economies", "Developed economies")) %>% 
  select(country, GDP_2015, GDP)
#doughnuts$GDP_2015 = round(doughnuts$GDP_2015*100/sum(doughnuts$GDP_2015))
doughnuts$GDP = round(doughnuts$GDP*100/sum(doughnuts$GDP))
doughnuts$population = pop %>% 
  filter(year==2024 & country %in% c("Developing economies", "Developed economies")) %>% 
  pull(population)
#doughnuts$population = round(doughnuts$population*100/ sum(doughnuts$population))
#doughnuts$vote_share = round(c(35.92, 64.02))
doughnuts$IMF_votes = c(1814540, 3226512)

write.csv(doughnuts, file = "SG_report_gdp_pop_imf_votes.csv")



compare_GDPs = compare_GDPs %>% filter(Year==2023)
compare_GDPs = compare_GDPs %>% 
  left_join(GDP_raw %>% filter(year==2023), by=c("ISO"="ISO3")) %>% 
  rename()

compare_GDPs$GDP_2015= compare_GDPs$GDP_2015 / 1000
compare_GDPs$GDP.y = compare_GDPs$GDP.y / 1000


plot <- ggplot(compare_GDPs, aes(
  x = GDP.x, 
  y = GDP_2015, 
  colour= unctad_development_status.x, 
  group = unctad_development_status.x,  
  text = paste(
    "Country:", country, 
    "<br>Development Status:", unctad_development_status.x,
    "<br>WEO (2024$):", round(GDP.x, 1),
    "<br>UNCTADstat (2015$):", round(GDP_2015, 1)
  )
)) +
  geom_point(size = 3, alpha = 0.7) +  # Custom point size and transparency
  geom_smooth(method = "lm", linetype = "dashed", size = 1) +  # Add a trend line
  labs(
    title = "GDP in 2023 by Country (Billions)",
    x = "WEO (2024$)",
    y = "UNCTADstat (2015$)",
    color = "Development Status"  # Legend title for color
  ) +
  theme_minimal(base_size = 14) +  # Clean and minimal theme with adjusted font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centered and bold title
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),  # Axis title size
    panel.grid.minor = element_blank()  # Remove minor gridlines for clarity
  )

# Convert to plotly
plotly_plot <- ggplotly(plot, tooltip = "text")

# Display the plot
plotly_plot

htmlwidgets::saveWidget(plotly_plot, "GDP_UNCTAD_vs_WEO.html")
getwd()
