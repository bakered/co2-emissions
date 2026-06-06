library(rvest)
library(dplyr)
library(readr)

# URL of the page containing the tables
url <- "https://www.imf.org/en/About/executive-board/members-quotas"

# Read the HTML content of the webpage
page <- read_html(url)

# Extract all tables with the class "responsive"
tables <- page %>% html_elements("table.responsive")

# Loop through tables, convert to dataframes, and combine them
all_data <- lapply(tables, function(tbl) {
  tbl %>% 
    html_table() %>%  # Convert to dataframe
    as.data.frame() %>%
    setNames(make.unique(names(.))) %>%
    select(1:6) %>%
    slice(-1:-2)
}) %>%
  bind_rows() # Combine all tables into one dataframe

colnames(all_data) <- tables[[1]] %>% 
  html_table() %>% 
  as.data.frame() %>% 
  slice(1:2) %>% 
  t() %>% 
  apply(1, function(x) paste(x[1], x[2])) %>% 
  gsub("[[:space:]]+", " ", .) %>%  # Removes extra whitespace
  gsub("\\r\\n", "", .) %>% 
  trimws()

all_data$Member <- gsub("\\d", "", all_data$Member)  # Remove all digits
all_data$Member <- trimws(all_data$Member)  # Remove trailing and leading whitespace


key <- read_csv("multi_country_key.csv")
key$practical_readable[key$ISO3=="CIV" & !is.na(key$ISO3)] = "Côte D'Ivoire"
key$practical_readable[key$ISO3=="TUR" & !is.na(key$ISO3)] = "Türkiye"

matches <- sapply(key, function(col) {
  sum(all_data$Member %in% col)
})
match_results <- data.frame(Column = names(matches), Matches = matches)
print(match_results %>% arrange(desc(Matches)))

rename_country = c(
  "Afghanistan, Islamic Republic of" = "Afghanistan", 
  "Congo, Democratic Republic of the" = "Congo, Dem. Rep.", 
  "Congo, Republic of"= "Congo, Rep.", 
  "Côte d'Ivoire"= "Cote d'Ivoire",
  "Czechia"= "Czech Republic",
  "Egypt"= "Egypt, Arab Rep.",
  "Fiji, Republic of"= "Fiji",
  "Iran, Islamic Republic of"= "Iran, Islamic Rep.",
  "Korea"= "Korea, Rep.",
  "Kosovo"= "Kosovo",
  "Lao People's Democratic Republic"= "Lao PDR",
  "Micronesia, Federated States of"= "Micronesia, Fed. Sts.", 
  "São Tomé and Príncipe"= "Sao Tome and Principe", 
  "South Sudan, Republic of"= "South Sudan", 
  "Türkiye"= "Turkey",
  "Venezuela, República Bolivariana de"= "Venezuela, RB",
  "Yemen, Republic of"= "Yemen, Rep."
)


all_data = all_data %>% 
  select(1:6) %>% 
  mutate(Member = recode(Member, !!!rename_country)) %>% 
  left_join(key %>% select(WDI, unctad_development_status), by=c('Member'='WDI')) 
all_data %>% filter(unctad_development_status %>% is.na()) %>% pull(Member) %>% dput()
all_data$unctad_development_status[all_data$Member=="Kosovo"] = "developed"

all_data = all_data %>%
  rename(vote_share = `VOTES Percent  of Total1`,
         votes = `VOTES Number2`,
         quote_share = `QUOTA Percent  of Total1`) %>% 
  mutate(vote_share = as.numeric(vote_share),
         votes = as.numeric(gsub(",", "", votes)),
         quote_share = as.numeric(quote_share)) 

IMF_votes = all_data %>%
  group_by(unctad_development_status) %>% 
  summarise(vote_share= sum(vote_share),
            votes= sum(votes),
            quote_share=sum(quote_share)) %>%
  ungroup()
    
rename_GDP_to_match_unctad = c(
  "Dem. Rep. of the Congo" = "Congo, Dem. Rep. of the", 
  "Cote d'Ivoire" = "C�te d'Ivoire", 
  "Republic of Korea" = "Korea, Republic of", 
  "Kosovo" = "Kosovo", 
  "Republic of Moldova" ="Moldova, Republic of", 
  "Netherlands (Kingdom of the)" ="Netherlands", 
  "Switzerland" ="Switzerland, Liechtenstein", 
  "United Republic of Tanzania" ="Tanzania, United Republic of", 
  "Turkiye" ="T�rkiye"
)
# GDP
GDP_raw <- open_7z("US.GDPTotal")
GDP_raw <- GDP_raw %>% 
  rename(GDP_2015=US..at.constant.prices..2015..in.millions, 
         country=Economy.Label, year=Year,
         GDP=US..at.current.prices.in.millions) %>% 
  mutate(country = ifelse(country == "United States of America including Puerto Rico", "United States of America", country)) %>% 
  mutate(country = recode(country, !!!rename_GDP_to_match_unctad)) %>%
  select(country, year, GDP_2015, GDP) %>% 
  left_join(key %>% select(ISO3, UNCTAD, unctad_development_status, WDI), by=c("country"="UNCTAD")) %>% 
  tibble()

plot_df = all_data %>% 
  left_join(GDP_raw %>% filter(year==2023), by=c("Member"="WDI")) %>% 
  select(Member, GDP_2015, GDP, votes)

plot_df %>% filter(GDP_2015 %>% is.na()) %>% pull(Member) %>% dput()

plot_df <- plot_df %>%
  mutate(
    GDP_share = GDP / sum(GDP, na.rm = TRUE) * 100,  
    votes_share = votes / sum(votes, na.rm = TRUE) * 100  
  )

library(ggplot2)
library(plotly)
library(htmlwidgets)
library(scales)  # For rounding to one significant figure

# Create the ggplot with raw data
plot <- ggplot(plot_df, aes(x = GDP_share, y = votes_share, text = paste("Member:", Member, 
                                                                  "<br>GDP Share (%):", round(GDP_share, 1), 
                                                                  "<br>Votes Share (%):", round(votes_share, 1)))) +
  geom_point(alpha = 1, size = 4) +  
  labs(
    title = "IMF Votes Share vs GDP Share",
    x = "GDP (2015 $ Market Rates)",
    y = "IMF Votes"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Equality line
  theme_minimal() +  # Clean minimal theme
  theme(
    plot.title = element_text(hjust = 0),  # Center the title
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14)  # Adjust axis titles size
  )

# Convert to interactive Plotly plot
plotly_plot <- ggplotly(plot, tooltip = "text")


# Show the plot
plotly_plot

# Save the plot as an HTML file
htmlwidgets::saveWidget(plotly_plot, "IMF_votes_GDP.html")

write_csv(plot_df, file="IMF_votes_GDP.csv")

#### do the same for population 
pop <- open_7z("US.PopTotal")
pop <- pop %>% 
  rename(population=Absolute.value.in.thousands, country=Economy.Label, year=Year) %>% 
  mutate(country = ifelse(country == "United States", "United States of America", country)) %>% 
  filter(year==2024) %>% 
  select(country, population) %>% 
  mutate(population = population * 1000) %>% # population is originally in thousands
  mutate(country = recode(country, !!!rename_GDP_to_match_unctad)) %>%
  left_join(key %>% select(ISO3, UNCTAD, unctad_development_status, WDI), by=c("country"="UNCTAD")) %>% 
  tibble()

plot_df = all_data %>% 
  left_join(pop, by=c("Member"="WDI")) %>% 
  select(Member, population, votes)

plot_df %>% filter(population %>% is.na()) %>% pull(Member) %>% dput()

plot_df <- plot_df %>%
  mutate(
    population_share = population / sum(population, na.rm = TRUE) * 100,  # 
    votes_share = votes / sum(votes, na.rm = TRUE) * 100  # Raw Votes share (percentage)
  )

# Create the ggplot with raw data
plot <- ggplot(plot_df, aes(x = population_share, y = votes_share, text = paste("Member:", Member, 
                                                                              "<br>Pop Share (%):", population_share, 
                                                                              "<br>Votes Share (%):", votes_share,
                                                                              "<br>Populatiion:", population, 
                                                                              "<br>IMF Votes:", votes))) +
  geom_point(alpha = 1, size = 4) +  
  labs(
    title = "IMF Votes Share vs Population Share",
    x = "Population Share",
    y = "IMF Vote Share"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Equality line
  theme_minimal() +  # Clean minimal theme
  theme(
    plot.title = element_text(hjust = 0),  # Center the title
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14)  # Adjust axis titles size
  )

# Convert to interactive Plotly plot
plotly_plot <- ggplotly(plot, tooltip = "text")


# Show the plot
plotly_plot

# Save the plot as an HTML file
htmlwidgets::saveWidget(plotly_plot, "IMF_votes_population.html")

write_csv(plot_df, file = "IMF_votes_population.csv")
