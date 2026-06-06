library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)


key = read.csv("/Users/edbaker/UN_projects/c02emmisions/multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"


gdp_data_raw <- read_excel("gdp_forecasts/WEOOct2024all.xls", na = "n/a")

gdp_data_pre = gdp_data_raw %>% 
  filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units %in% c("Purchasing power parity; international dollars")) %>% 
  mutate(across(`1980`:`2029`, as.numeric)) %>% 
  pivot_longer(
    cols = `1980`:`2029`,   # Specify the range of columns for the years
    names_to = "Year",       # Name for the new column that will hold the years
    values_to = "GDP_PPP"      # Name for the new column that will hold the values
  ) %>% 
  select(ISO, Country, Year, GDP_PPP)
gdp_data_pre_market = gdp_data_raw %>% 
  filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units %in% c("U.S. dollars")) %>% 
  mutate(across(`1980`:`2029`, as.numeric)) %>% 
  pivot_longer(
    cols = `1980`:`2029`,   # Specify the range of columns for the years
    names_to = "Year",       # Name for the new column that will hold the years
    values_to = "GDP_market"      # Name for the new column that will hold the values
  ) %>% 
  select(ISO, Year, GDP_market)
gdp_data_pre = gdp_data_pre %>% 
  left_join(gdp_data_pre_market, by = c("ISO", "Year"))

gdp_data_pre %>%
  filter(Year%in%c(2024:2029)) %>% 
  group_by(Country) %>%
  summarize(na_count = sum(is.na(GDP_PPP))) %>% 
  filter(na_count != 0) %>% 
  arrange(desc(na_count)) %>% 
  print(n=55)

# need to deal with c("Kosovo", "Puerto Rico", "West Bank and Gaza")
gdp_data = gdp_data_pre %>% 
  left_join(key %>% select(UNCTAD_region, unctad_development_status, ISO3, ISO2, practical_readable), by=c("ISO"="ISO3")) %>% 
  rename(region=UNCTAD_region,
         country_name=practical_readable) %>% 
  mutate(
    region1 = case_when(
      Country == "Kosovo" ~ "Developed",
      Country == "Puerto Rico" ~ "Developed",
      Country == "West Bank and Gaza" ~ "Developing Asia and Oceania",
      unctad_development_status == "developed" ~ "Developed",
      region == "Africa" ~ "Africa",
      region == "America" ~ "Latin America and the Caribbean",
      region == "Asia and Oceania" ~ "Developing Asia and Oceania",
      TRUE ~ "other"
    )) 
compare_GDPs = gdp_data
gdp_data %>% 
  filter(Year %in% c(2024:2029)) %>% 
  filter(region1 == "other" | is.na(region1) | is.na(GDP_PPP))  %>%  pull(Country) %>% unique() %>% dput()
gdp_data = gdp_data
  group_by(region1, Year) %>% 
  summarise(GDP_PPP = sum(GDP_PPP, na.rm = TRUE),
            GDP_market = sum(GDP_market, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Year = as.integer(Year),
         GDP_PPP = GDP_PPP/1000,
         GDP_market = GDP_market/1000) #into trillions

gdp_data_world = gdp_data %>% 
  group_by(Year) %>% 
  summarise(GDP_PPP = sum(GDP_PPP, na.rm = TRUE)) %>% 
  ungroup()

next_5_years = gdp_data %>% 
  filter(Year %in% c(2024,2029)) %>% 
  group_by(region1) %>% 
  summarise(
    GDP_2024 = GDP_PPP[Year == 2024],
    GDP_2029 = GDP_PPP[Year == 2029],
    gdp_diff = sum(GDP_PPP[Year == 2029], na.rm = TRUE) - sum(GDP_PPP[Year == 2024], na.rm = TRUE),
    .groups = "drop" # Ungroup after summarising
  )
 
ggplot() +
  # First layer: before 2024, alpha = 0.8
  geom_area(data = filter(gdp_data, Year <= 2024), aes(x = Year, y = GDP_PPP, group = region1, fill = region1), alpha = 0.8) + 
  # Second layer: after 2024, alpha = 0.5
  geom_area(data = filter(gdp_data, Year >= 2024), aes(x = Year, y = GDP_PPP, group = region1, fill = region1), alpha = 0.5) +  
  # Add the vertical line at 2024
  geom_vline(xintercept = 2024, linetype = "dotted", color = "red") +  
  theme_minimal() + 
  labs(x = "Year", y = "GDP PPP (trillion $)", title = "GDP by Region (PPP)") +
  theme(legend.position = "bottom")

data_datawrapper = gdp_data %>% pivot_wider(
  names_from = Year,         # Specify the column to pivot to wide (Year)
  values_from = GDP_PPP          # Specify the values to fill the new columns (GDP)
) %>% t() %>% 
  as_tibble(rownames = "Year", .name_repair = "unique")
colnames(data_datawrapper) <- data_datawrapper[1, ]
colnames(data_datawrapper)[1] = "Year"
data_datawrapper <- data_datawrapper[-1, ] %>%
  type.convert(as.is = TRUE)

gdp_data_next_5 <-  data_datawrapper %>%
  filter(Year %in% c(2024, 2029)) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "GDP_PPP") %>%
  pivot_wider(names_from = Year, values_from = GDP_PPP) %>%
  mutate(`GDP Growth` = `2029` - `2024`) %>%
  select(Region, `GDP Growth`) %>% 
  arrange(desc(`GDP Growth`))

#### now for constant dollars 
ggplot() +
  # First layer: before 2024, alpha = 0.8
  geom_area(data = filter(gdp_data, Year <= 2024), aes(x = Year, y = GDP_market, group = region1, fill = region1), alpha = 0.8) + 
  # Second layer: after 2024, alpha = 0.5
  geom_area(data = filter(gdp_data, Year >= 2024), aes(x = Year, y = GDP_market, group = region1, fill = region1), alpha = 0.5) +  
  # Add the vertical line at 2024
  geom_vline(xintercept = 2024, linetype = "dotted", color = "red") +  
  theme_minimal() + 
  labs(x = "Year", y = "GDP (trillion $)", title = "GDP by Region (Current market US $)") +
  theme(legend.position = "bottom")

data_datawrapper = gdp_data %>% pivot_wider(
  names_from = Year,         # Specify the column to pivot to wide (Year)
  values_from = GDP_market          # Specify the values to fill the new columns (GDP)
) %>% t() %>% 
  as_tibble(rownames = "Year", .name_repair = "unique")
colnames(data_datawrapper) <- data_datawrapper[1, ]
colnames(data_datawrapper)[1] = "Year"
data_datawrapper <- data_datawrapper[-1, ] %>%
  type.convert(as.is = TRUE)

gdp_data_next_5_2 <-  data_datawrapper %>%
  filter(Year %in% c(2024, 2029)) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "GDP_market") %>%
  pivot_wider(names_from = Year, values_from = GDP_market) %>%
  mutate(`GDP_market Growth` = `2029` - `2024`) %>%
  select(Region, `GDP_market Growth`) %>% 
  arrange(desc(`GDP_market Growth`))

if(F){
library(DatawRappr)
library(rdwd)

# set the token
datawrapper_auth(api_key = Sys.getenv("DATAWRAPPER_TOKEN"), overwrite = TRUE)

# make sure the key is working as expected
dw_test_key()

dw_create_chart(        
  type = 'd3-area'
)

data_datawrapper = gdp_data %>% pivot_wider(
  names_from = Year,         # Specify the column to pivot to wide (Year)
  values_from = GDP          # Specify the values to fill the new columns (GDP)
) %>% t() %>% 
  as_tibble(rownames = "Year", .name_repair = "unique")
colnames(data_datawrapper) <- data_datawrapper[1, ]
colnames(data_datawrapper)[1] = "Year"
data_datawrapper <- data_datawrapper[-1, ] %>%
  type.convert(as.is = TRUE)
  

dw_data_to_chart(    
  data_datawrapper,    
  chart_id = "xD9CC"  
)
dw_retrieve_chart_metadata("xD9CC")

dw_edit_chart(chart_id = "xD9CC",
              type = "d3-area",
              title="GDP (PPP) Forecast",
              visualize = list(
                axes = list(
                  x = "Year",   # Name of the X-axis column in your data
                  y = "GDP"     # Name of the Y-axis column in your data
              ),
              stacking = TRUE))

dw_export_chart("xD9CC")


dw_create_chart(        
  type = 'd3-area'
)
dw_retrieve_chart_metadata("DztEv")

dw_data_to_chart(    
  gdp_data_world,    
  chart_id = "DztEv"  
)
dw_retrieve_chart_metadata("DztEv")

dw_edit_chart(chart_id = "DztEv",
              type = "d3-area",
              title="GDP (PPP) Forecast",
              visualize = list(
                axes = list(
                  x = "Year",   # Name of the X-axis column in your data
                  y = "GDP"     # Name of the Y-axis column in your data
                )))

dw_export_chart("DztEv")

gdp_data_next_5 <-  data_datawrapper %>%
  filter(Year %in% c(2024, 2029)) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "GDP") %>%
  pivot_wider(names_from = Year, values_from = GDP) %>%
  mutate(`GDP Growth` = `2029` - `2024`) %>%
  select(Region, `GDP Growth`) %>% 
  arrange(desc(`GDP Growth`))

dw_create_chart(        
  type = 'd3-bars'
)
dw_data_to_chart(    
  gdp_data_next_5,    
  chart_id = "8QfOO"  
)
dw_export_chart("8QfOO")
}
