

library(tidyverse)
library(WDI)
library(dplyr)
library(ggrepel)

tariff_rev_raw = WDI(indicator = "GC.TAX.INTT.RV.ZS", country = "all")  #GC.TAX.INTT.RV.ZS is Taxes on international trade (% of revenue)

key <- read.csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"


tariff_rev_most_recent_year = tariff_rev_raw %>% 
  rename("value" = GC.TAX.INTT.RV.ZS) 
  filter(year>2015 & !is.na(value))  %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>%
  select(iso3c, year, value) %>% 
  filter(iso3c != "" & iso3c != "WLD") %>% 
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), by=c("iso3c"="ISO3")) %>% 
  select(UNCTAD_code, year, value) %>% 
  mutate(UNCTAD_code = str_pad(UNCTAD_code, width = 3, side = "left", pad = "0")) %>% 
  rename("Taxes on International Trade" = value) 

write.csv(tariff_rev_most_recent_year, row.names = FALSE)

tariff_rev_raw %>% 
  filter(!is.na(GC.TAX.INTT.RV.ZS))  %>%
  select(iso3c, year, GC.TAX.INTT.RV.ZS) %>% 
  filter(iso3c != "" & iso3c != "WLD") %>% 
  ggplot(aes(y=GC.TAX.INTT.RV.ZS, x=year, group=iso3c, colour=iso3c)) +
  geom_line()

tariff_by_dev_status <- tariff_rev_raw %>% 
  filter(!is.na(GC.TAX.INTT.RV.ZS)) %>%  # Remove NAs
  mutate(year_group = floor(year / 5) * 5) %>%  # Create 5-year groups
  group_by(country, iso3c, year_group) %>% 
  slice_max(year, with_ties = FALSE) %>%  # Get the most recent year per 5-year group
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), 
            by = c("iso3c" = "ISO3")) %>%  # Merge with key dataset
  group_by(unctad_development_status, year_group) %>% 
  summarise(mean_tariff_revenue = mean(GC.TAX.INTT.RV.ZS, na.rm = TRUE)) %>%  # Compute mean per group
  ungroup() %>% 
  filter(!is.na(unctad_development_status) & unctad_development_status != "disuse")

ggplot(tariff_by_dev_status, aes(x = year_group, y = mean_tariff_revenue, color = unctad_development_status)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "International Trade Tax as % of Government Revenue",
       x = "Year",
       y = NULL,
       color = "Development Status") +
  theme_minimal()



  
####### NEW INDICATOR
tariff_rev_raw2 = WDI(indicator = "GC.TAX.IMPT.ZS", country = "all")  #GC.TAX.IMPT.ZS is Customs and other import duties (% of tax revenue)
lat_lon <- read.csv("~/messing around/lat_lon.csv")

tariff_rev_most_recent_year2 = tariff_rev_raw2 %>% 
  rename("value" = GC.TAX.IMPT.ZS) %>% 
  #filter(year>2015 & !is.na(value))  %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>%
  filter(iso3c != "" & iso3c != "WLD") %>% 
  left_join(key %>% select(ISO3, UNCTAD, datawrapper_maps, unctad_development_status, datawrapper_flags), by=c("iso3c"="ISO3")) %>% 
  left_join(lat_lon, by=c("datawrapper_flags"="DW")) %>% 
  select(country, datawrapper_maps, unctad_development_status, year, value, LAT, LON.1) %>% 
  mutate(datawrapper_maps = str_pad(datawrapper_maps, width = 3, side = "left", pad = "0")) %>% 
  rename("Customs and other import duties (% of tax revenue)" = value) 

write.csv(tariff_rev_most_recent_year2, row.names = FALSE)
write.csv(tariff_rev_most_recent_year2, "tariff_rev_as_share_of_tax_rev.csv", row.names = FALSE)

customs_by_dev_status <- tariff_rev_raw2 %>% 
  rename("value" = GC.TAX.IMPT.ZS) %>% 
  filter(!is.na(value)) %>%  # Remove NAs
  mutate(year_group = floor(year / 5) * 5) %>%  # Create 5-year groups
  group_by(country, iso3c, year_group) %>% 
  slice_max(year, with_ties = FALSE) %>%  # Get the most recent year per 5-year group
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), 
            by = c("iso3c" = "ISO3")) %>%  # Merge with key dataset
  group_by(unctad_development_status, year_group) %>% 
  summarise(mean_tariff_revenue = mean(value, na.rm = TRUE)) %>%  # Compute mean per group
  ungroup() %>% 
  filter(!is.na(unctad_development_status) & unctad_development_status != "disuse")

ggplot(tariff_by_dev_status, aes(x = year_group, y = mean_tariff_revenue, color = unctad_development_status)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Customs and other import duties (% of tax revenue)",
       x = "Year",
       y = NULL,
       color = "Development Status") +
  theme_minimal()


library(dplyr)

# Identify countries with a potential anomaly in the most recent GC.TAX.IMPT.ZS value
anomalous_countries <- tariff_rev_raw2 %>%
  filter(!is.na(GC.TAX.IMPT.ZS)) %>%  # Remove NAs
  group_by(country) %>%
  slice_max(year, n = 2, with_ties = FALSE) %>%  # Get the two most recent data points
  arrange(country, desc(year)) %>%  # Ensure correct ordering
  summarise(
    latest_value = first(GC.TAX.IMPT.ZS),
    prev_value = last(GC.TAX.IMPT.ZS),
    change_pct = (latest_value - prev_value) / prev_value * 100,
    change_pct_points = (latest_value - prev_value),
    .groups = "drop"
  ) %>%
  filter(change_pct > 20 | change_pct < -20) %>%  # Keep only those with >10% change
  filter(change_pct_points>5 | change_pct_points< -5) 


#  pull(country)  # Return only the list of country names

anomalous_countries <- tariff_rev_raw2 %>%
  filter(year>2015) %>% 
  filter(!is.na(GC.TAX.IMPT.ZS)) %>%  # Remove NAs
  group_by(country) %>%
  slice_max(year, n = 6, with_ties = FALSE) %>%  # Get the last 6 years (latest + 5 previous)
  arrange(country, desc(year)) %>%  # Ensure correct ordering (latest first)
  mutate(rank = row_number()) %>%  # Assign rank to identify latest vs previous 5
  summarise(
    latest_value = first(GC.TAX.IMPT.ZS),  # Most recent value
    prev_value = nth(GC.TAX.IMPT.ZS, 2),  # Second most recent value
    change_pct = (latest_value - prev_value) / prev_value * 100,  # % change from previous year
    change_pct_points = latest_value - prev_value,  # Absolute change in percentage points
    avg_last_5 = mean(GC.TAX.IMPT.ZS[rank > 1], na.rm = TRUE),  # Average of previous 5 values
    deviation_from_avg = (latest_value - avg_last_5) / avg_last_5 * 100,  # % deviation from avg
    deviation_points = latest_value - avg_last_5,  # Absolute deviation from avg
    .groups = "drop"
  ) %>%
  filter(abs(change_pct) > 20 & abs(change_pct_points) > 5) 


anomalous_countries <- tariff_rev_raw2 %>%
  filter(!is.na(GC.TAX.IMPT.ZS)) %>%  # Remove NAs
  group_by(country) %>%
  slice_max(year, n = 6, with_ties = FALSE) %>%  # Get the last 6 years (latest + 5 previous)
  arrange(country, desc(year)) %>%  # Ensure correct ordering (latest first)
  mutate(rank = row_number()) %>%  # Assign a rank based on recency
  pivot_wider(
    names_from = rank,
    values_from = GC.TAX.IMPT.ZS,
    names_prefix = "latest"
  ) %>% 
  rename(
    latest6th = latest6,  # Rename for clarity
    latest5th = latest5,
    latest4th = latest4,
    latest3rd = latest3,
    latest2nd = latest2,
    latest = latest1
  ) %>%
  mutate(
    change_pct = (latest - latest2nd) / latest2nd * 100,  # % change from latest to latest2nd
    change_pct_points = latest - latest2nd  # Absolute change in percentage points
  ) %>%
  filter(abs(change_pct) > 20 | abs(change_pct_points) > 5)  # Keep only significant changes

# Filter and rename the data
tariff_rev_filtered <- tariff_rev_raw2 %>%
  filter(year > 2015) %>%
  filter(!is.na(GC.TAX.IMPT.ZS)) %>%
  rename(tariff_as_share_tax = GC.TAX.IMPT.ZS) %>% 
  filter(iso3c != "" & iso3c != "WLD") %>% 
  left_join(key %>% select(ISO3, UNCTAD, SIDS_UNCTAD, LLDC, LDC, unctad_development_status), by=c("iso3c"="ISO3"))


# Plot the data
ggplot(tariff_rev_filtered, aes(x = year, y = tariff_as_share_tax, group = country, color = unctad_development_status)) +
  geom_line(alpha = 0.5, size = 0.4) +  # Thin and slightly transparent lines
  theme_minimal() +
  labs(title = "Tariff Revenue as a Share of Tax Revenue (2015+)",
       x = "Year",
       y = "Tariff Revenue (% of Tax Revenue)",
       color = "Country") +
  theme(legend.position = "none")  # Hide the legend since 150 countries would clutter it

library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)

# Prepare data without grouping by 'group'
tariff_group_means <- tariff_rev_filtered %>%
  mutate(across(c(SIDS_UNCTAD, LLDC, LDC, unctad_development_status), as.character)) %>%
  pivot_longer(cols = c(SIDS_UNCTAD, LLDC, LDC, unctad_development_status),
               names_to = "group_type", values_to = "group_membership") %>%
  filter(!is.na(group_membership)) %>%
  mutate(group = case_when(
    group_type == "LDC" & group_membership == "0" ~ "non-LDC",
    group_type == "LDC" & group_membership == "1" ~ "LDC",
    group_type == "LLDC" & group_membership == "0" ~ "non-LLDC",
    group_type == "LLDC" & group_membership == "1" ~ "LLDC",
    group_type == "SIDS_UNCTAD" & group_membership == "0" ~ "non-SIDS",
    group_type == "SIDS_UNCTAD" & group_membership == "1" ~ "SIDS",
    group_type == "unctad_development_status" & group_membership == "developed" ~ "developed",
    group_type == "unctad_development_status" & group_membership == "developing" ~ "developing",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group) & !str_detect(group, "non"))  # Remove non-groups

# Create ggplot with country tooltips
p <- ggplot(tariff_group_means, aes(x = year, y = tariff_as_share_tax, group=group, color = group, text = country)) +
  geom_point(alpha = 0.4, size = 1.5) +  # Scatter plot for individual data points
  geom_smooth(method = "loess", se = TRUE, size = 1.2) +  # Locally smoothed trend line
  theme_minimal() +
  labs(title = "Tariff Revenue as a Share of Tax Revenue",
       subtitle = "Interactive Scatter Plot with Local Regression",
       x = "Year",
       y = "Tariff Revenue (% of Tax Revenue)",
       color = "Group") +
  theme(legend.position = "right")

# Convert to interactive Plotly chart with tooltips
ggplotly(p, tooltip = "text")



#### NEW DATA 
library(WDI)
library(dplyr)
library(stringr)

# Pull WDI data: customs revenue (LCU) and exchange rate (LCU/USD)
indicators <- c("GC.TAX.IMPT.CN", "PA.NUS.FCRF")
wdi_data <- WDI(indicator = indicators, start = 2015, end = as.numeric(format(Sys.Date(), "%Y")), extra = TRUE)
lat_lon <- read.csv("~/messing around/lat_lon.csv")


wdi_df <- wdi_data %>%
  rename(
    customs_revenue_lcu = GC.TAX.IMPT.CN,
    exchange_rate = PA.NUS.FCRF
  ) %>%
  mutate(
    value = customs_revenue_lcu / exchange_rate
  ) %>%
  filter(year > 2015 & !is.na(value)) %>%
  group_by(country, iso3c) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup() %>%
  filter(iso3c != "" & iso3c != "WLD") %>%
  left_join(key %>% select(ISO3, UNCTAD, datawrapper_maps, unctad_development_status, datawrapper_flags), by=c("iso3c"="ISO3")) %>% 
  left_join(lat_lon, by=c("datawrapper_flags"="DW")) %>% 
  select(country, datawrapper_maps, unctad_development_status, year,customs_revenue_lcu, exchange_rate, value, LAT, LON.1) %>% 
  mutate(datawrapper_maps = str_pad(datawrapper_maps, width = 3, side = "left", pad = "0")) %>% 
  rename("Customs Revenue (USD)" = value)

# View result
print(wdi_df)
write.csv(wdi_df, row.names = FALSE)
write.csv(wdi_df, file="total_tariff_revenue_usd_by_country.csv", row.names = FALSE)


