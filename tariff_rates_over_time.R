library(tidyverse)
library(WDI)
library(dplyr)

tariff_rates_raw = WDI(indicator = c("TM.TAX.MRCH.WM.AR.ZS", "TM.VAL.MRCH.CD.WT"), country = "all")  
#TM.TAX.MRCH.WM.AR.ZS is Weighted mean applied tariff is the average of effectively applied rates weighted by the product import shares corresponding to each partner country.
#TM.VAL.MRCH.CD.WT is Merchandise imports show the c.i.f. value of goods received from the rest of the world valued in current U.S. dollars.
write.csv(tariff_rates_raw, "tariff_rates_raw.csv", row.names = FALSE)

key <- read.csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"


subset_countries_old = c("Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", 
                     "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas, The", 
                     "Bahrain", "Bangladesh", "Belarus", "Belgium", "Belize", "Benin", 
                     "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", 
                     "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", 
                     "Canada", "Central African Republic", "Chad", "Chile", "China", 
                     "Colombia", "Congo, Dem. Rep.", "Congo, Rep.", "Costa Rica", 
                     "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", 
                     "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Estonia", 
                     "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", 
                     "Gambia, The", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                     "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
                     "Hong Kong SAR, China", "Hungary", "Iceland", "India", "Indonesia", 
                     "Iran, Islamic Rep.", "Ireland", "Israel", "Italy", "Jamaica", 
                     "Japan", "Jordan", "Kazakhstan", "Kenya", "Korea, Rep.", "Kuwait", 
                     "Kyrgyz Republic", "Lao PDR", "Latvia", "Lebanon", "Lesotho", 
                     "Lithuania", "Macao SAR, China", "Madagascar", "Malawi", "Malaysia", 
                     "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", 
                     "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                     "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", 
                     "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Panama", 
                     "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", 
                     "Portugal", "Qatar", "Romania", "Russian Federation", "Rwanda", 
                     "Saudi Arabia", "Senegal", "Singapore", "Slovak Republic", "Slovenia", 
                     "South Africa", "Spain", "Sri Lanka", "St. Kitts and Nevis", 
                     "St. Lucia", "St. Vincent and the Grenadines", "Sudan", "Suriname", 
                     "Sweden", "Switzerland", "Tanzania", "Thailand", "Togo", "Turkiye", 
                     "Uganda", "Ukraine", "United Kingdom", "United States", "Uruguay", 
                     "Uzbekistan", "Vanuatu", "Venezuela, RB", "Viet Nam", "Zambia", 
                     "Zimbabwe")

subset_countries = c("Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", 
                     "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas, The", 
                     "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
                     "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", 
                     "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", 
                     "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", 
                     "Chad", "Chile", "China", "Colombia", "Costa Rica", "Cote d'Ivoire", 
                     "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Dominican Republic", 
                     "Ecuador", "El Salvador", "Estonia", "Eswatini", "Ethiopia", 
                     "Fiji", "Finland", "France", "Gabon", "Gambia, The", "Georgia", 
                     "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", 
                     "Guinea-Bissau", "Guyana", "Honduras", "Hong Kong SAR, China", 
                     "Hungary", "Iceland", "India", "Indonesia", "Ireland", "Israel", 
                     "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
                     "Korea, Rep.", "Kuwait", "Kyrgyz Republic", "Lao PDR", "Latvia", 
                     "Lesotho", "Lithuania", "Macao SAR, China", "Madagascar", "Malawi", 
                     "Malaysia", "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", 
                     "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", 
                     "Namibia", "Nepal", "Netherlands", "New Zealand", "Nicaragua", 
                     "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", 
                     "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
                     "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", 
                     "Rwanda", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", 
                     "Singapore", "Slovak Republic", "Slovenia", "South Africa", "Spain", 
                     "Sri Lanka", "St. Vincent and the Grenadines", "Sudan", "Sweden", 
                     "Switzerland", "Tanzania", "Togo", "Turkiye", "Uganda", "Ukraine", 
                     "United Arab Emirates", "United Kingdom", "United States", "Uruguay", 
                     "Uzbekistan", "Vanuatu", "Venezuela, RB", "Viet Nam", "Zambia", 
                     "Zimbabwe")
# Countries that were in the old list but not in the new one
setdiff(subset_countries_old, subset_countries)
# Countries that are new in the current list (not in the old one)
setdiff(subset_countries, subset_countries_old)


tariff_rates_value <- tariff_rates_raw %>% 
  filter(country %in% subset_countries) %>% 
  rename("value" = TM.TAX.MRCH.WM.AR.ZS, "weight" = TM.VAL.MRCH.CD.WT) %>% 
  mutate(year_group = ceiling(year / 5) * 5) %>%   # Create 5-year groups
  filter(!is.na(value)) %>%  # Remove NAs
  group_by(country, iso3c, year_group) %>% 
  slice_max(year, with_ties = FALSE) %>%  # Get the most recent year per 5-year group
  ungroup() %>% 
  select(country, iso3c, year_group, value)
tariff_rates_weight <- tariff_rates_raw %>% 
  filter(country %in% subset_countries) %>% 
  rename("value" = TM.TAX.MRCH.WM.AR.ZS, "weight" = TM.VAL.MRCH.CD.WT) %>% 
  mutate(year_group = (floor(year / 5) * 5) + 5) %>%  # Create 5-year groups
  filter(!is.na(weight)) %>%  # Remove NAs
  group_by(country, iso3c, year_group) %>% 
  slice_max(year, with_ties = FALSE) %>%  # Get the most recent year per 5-year group
  ungroup() %>% 
  select(country, iso3c, year_group, weight)
tariff_rates = tariff_rates_value %>% 
  left_join(tariff_rates_weight, by=c("country", "iso3c", "year_group")) %>% 
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), 
            by = c("iso3c" = "ISO3")) %>%  # Merge with key dataset
  filter(!is.na(unctad_development_status) & unctad_development_status != "disuse" & !is.na(weight)) %>% 
  group_by(year_group) %>% 
  summarise(mean_tariff_rate = weighted.mean(value, w = weight, na.rm = TRUE),
            ncountries = n_distinct(country),  # Count unique countries
            country_list = list(unique(country))) %>%  # Compute mean per group
  ungroup()
  
# tariff_rates$country_list[3][!c( c(tariff_rates$country_list[3] %>% str_split(", "))[[1]] %in% c(tariff_rates$country_list[4] %>% str_split(", "))[[1]]) ]

# Reduce(intersect, tariff_rates$country_list[4:8]) %>%  dput() 

tariff_rates = tariff_rates %>% 
  filter(year_group>2000) 

tariff_rates$source = "WDI"

manual_tariff_rates <- data.frame(
  #unctad_development_status = rep("developed", 6),
  year_group = c(1947, 1948, 1964, 1972, 1987, 1999),
  mean_tariff_rate = c(22.0, 16.8, 14.0, 8.0, 5.0, 4.0),
  ncountries = "17?",
  country_list = c("us", "eu", "japan"),
  source = "Bown and Irwin 2015"
)

tariff_rates = rbind(tariff_rates, manual_tariff_rates)


library(ggrepel)
# Compute weighted average tariff per year
weighted_avg <- tariff_rates_raw %>%
  filter(!is.na(TM.TAX.MRCH.WM.AR.ZS), !is.na(TM.VAL.MRCH.CD.WT)) %>%
  group_by(year) %>%
  summarise(
    weighted_tariff = sum(TM.TAX.MRCH.WM.AR.ZS * TM.VAL.MRCH.CD.WT) / sum(TM.VAL.MRCH.CD.WT),
    .groups = "drop"
  )
# Plot
ggplot(weighted_avg, aes(x = year, y = weighted_tariff)) +
  geom_line(size = 1) +
  labs(
    title = "Global Weighted Average Tariff Rate Over Time",
    x = "Year",
    y = "Weighted Average Tariff Rate (%)"
  ) +
  theme_minimal()

topN <- tariff_rates_raw %>%
  filter(country %in% subset_countries) %>% 
  group_by(country) %>%
  summarise(total_import = sum(TM.VAL.MRCH.CD.WT, na.rm = TRUE), .groups = "drop") %>%
  top_n(10, total_import) %>%
  pull(country)

tariff_topN <- tariff_rates_raw %>%
  filter(country %in% topN, !is.na(TM.TAX.MRCH.WM.AR.ZS))

labels <- tariff_topN %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(tariff_topN, aes(x = year, y = TM.TAX.MRCH.WM.AR.ZS, color = country)) +
  geom_line() +
  geom_text_repel(
    data = labels,
    aes(label = country),
    segment.color = "grey70",
    size = 3,
    box.padding = 0.2,
    point.padding = 0.1,
    max.overlaps = Inf  # allow all labels
  ) +
  labs(
    title = "Tariff Rates Over Time (Top N Importers)",
    x = "Year",
    y = "Tariff Rate (% of Import Value)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")




ggplot(tariff_rates, 
       aes(x = year_group, 
           y = mean_tariff_rate)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ylim(0, NA) +
  labs(title = "Mean Applied Tariff Rates Across All Product Types",
       x = "Year",
       y = NULL,
       color = NULL,
       linetype = "Source") +  # Add legend label for line type
  theme_minimal()


ggplot(tariff_rates, 
       aes(x = year_group, 
           y = mean_tariff_rate, 
           color = source)) +  
  geom_line(size = 1, alpha = 0.85) +  # Thinner, semi-transparent line
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +  # White-filled points
  ylim(0, NA) +
  labs(title = "Mean Applied Tariff Rates Across All Product Types",
       x = "Year",
       y = "Tariff Rate (%)",
       color = NULL) +  # Remove legend title for cleaner look
  theme_minimal(base_size = 14) +  # Larger base font size
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold and centered title
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Add margin to Y-axis title
    axis.text = element_text(size = 12),  # Increase axis text size
    legend.position = "top",  # Move legend to the top
    legend.text = element_text(size = 12),  # Bigger legend text
    panel.grid.major = element_line(color = "grey90", size = 0.3),  # Light grey grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove border
    axis.line.x = element_line(color = "black"),  # Add X-axis line for clarity
    axis.line.y = element_line(color = "black")   # Add Y-axis line
  ) +
  scale_color_manual(values = c("unctadStat" = "#1f78b4",  # Datawrapper-like blue
                                "Bown & Irwin (2015)" = "#33a02c"))  # Datawrapper-like green

# Pivot the data to wide format
tariff_rates_wide <- tariff_rates %>%
  mutate(column_name = paste0(source)) %>% # Create new column names
  select(-source) %>%  # Remove unnecessary columns
  pivot_wider(names_from = column_name, values_from = mean_tariff_rate) %>% 
  arrange(year_group)
colnames(tariff_rates_wide)[1] = c("year")

write.csv(tariff_rates_wide, row.names = FALSE)
tariff_rates$country_list <- NULL
write.csv(tariff_rates, "mean:tarriff_rates.csv", row.names = FALSE)

## all countries









#### NEW INDICATOR

# TM.TAX.MRCH.SM.FN.ZS = Tariff rate, most favored nation, simple mean, all products (%)
tariff_rates_raw2 = WDI(indicator = "TM.TAX.MRCH.SM.FN.ZS", country = "all")  #TM.TAX.MRCH.SM.FN.ZS is

tariff_rates_by_dev_status2 <- tariff_rates_raw2 %>% 
  rename("value" = TM.TAX.MRCH.SM.FN.ZS) %>% 
  filter(!is.na(value)) %>%  # Remove NAs
  mutate(year_group = floor(year / 5) * 5) %>%  # Create 5-year groups
  group_by(country, iso3c, year_group) %>% 
  slice_max(year, with_ties = FALSE) %>%  # Get the most recent year per 5-year group
  left_join(key %>% select(ISO3, UNCTAD, UNCTAD_code, unctad_development_status), 
            by = c("iso3c" = "ISO3")) %>%  # Merge with key dataset
  group_by(unctad_development_status, year_group) %>% 
  summarise(mean_tariff_rate = mean(value, na.rm = TRUE)) %>%  # Compute mean per group
  ungroup() %>% 
  filter(!is.na(unctad_development_status) & unctad_development_status != "disuse")

ggplot(tariff_rates_by_dev_status2, aes(x = year_group, y = mean_tariff_rate, color = unctad_development_status)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ylim(0,NA) +
  labs(title = "Tariff rates",
       x = "Year",
       y = NULL,
       color = "Development Status") +
  theme_minimal()



