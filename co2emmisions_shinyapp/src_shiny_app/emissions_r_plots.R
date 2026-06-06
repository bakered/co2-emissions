## this code has been replaced by python plotlyplot1


# plot
p <- data %>% # filter(year==2020) %>% 
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
  #scale_x_log10() +
  scale_size_continuous(range = c(0.01, 50)) +
  labs(x="GDP in millions", y="Tons of CO2 emissions per capita", alpha="", color="", size="") +
  scale_y_continuous(limits = c(0, 65)) +
  theme_minimal()

p_plotly <- ggplotly(p, tooltip = "text")

# Maximum dimension and sizing adjustments
#data = data %>% filter(year==2020)
maxDim <- data %>% select(gdp, co2_per_capita) %>% summarise(across(everything(), max)) %>% which.max()
maxi <- data[[names(maxDim)]][which.max(data[[names(maxDim)]])]

# Function to generate image annotations for a specific year
generate_images_for_year <- function(year) {
  subset_data <- data %>% filter(year == year)
  image_annotations <- lapply(1:nrow(subset_data), function(i) {
    country <- subset_data$ISO2[i] %>% tolower()
    list(
      source = paste0("https://hatscripts.github.io/circle-flags/flags/", country, ".svg"),
      xref = "x",
      yref = "y",
      xanchor = "center",
      yanchor = "middle",
      x = subset_data$gdp[i],
      y = subset_data$co2_per_capita[i],
      sizex = (sqrt(subset_data$co2[i] / max(subset_data$co2)) * maxi * 0.2 + maxi * 0.05) * 0.5,
      sizey = (sqrt(subset_data$co2[i] / max(subset_data$co2)) * maxi * 0.2 + maxi * 0.05) * 0.5,
      sizing = "contain",
      opacity = 0.8,
      layer = "above"
    )
  })
  return(image_annotations)
}

# Build the Plotly plot object
plotly_build(p_plotly)

# Function to update the plotly object with images for each year
update_plotly_for_years <- function(plotly_obj, years) {
  frames <- list()
  for (year in years) {
    image_annotations <- generate_images_for_year(year)
    frames[[as.character(year)]] <- list(
      layout = list(images = image_annotations)
    )
  }
  plotly_obj %>%
    plotly::layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          x = 1.05,
          y = 0.5,
          buttons = lapply(years, function(year) {
            list(
              method = 'animate',
              args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE), fromcurrent = TRUE)),
              label = as.character(year)
            )
          })
        )
      ),
      sliders = list(
        list(
          steps = lapply(years, function(year) {
            list(
              method = 'animate',
              args = list(list(year), list(mode = 'immediate', frame = list(duration = 1000, redraw = TRUE), fromcurrent = TRUE)),
              label = as.character(year)
            )
          })
        )
      )
    ) %>%
    plotly::animation_opts(frame = 1000, redraw = TRUE) %>%
    plotly::animation_slider(currentvalue = list(prefix = "Year: "))
}

# Apply the update to the Plotly object
years <- unique(data$year)
updated_plotly <- update_plotly_for_years(p_plotly, years)

# Display the Plotly plot
updated_plotly

# Create a list to store image annotations
image_annotations <- lapply(1:nrow(data), function(i) {
  country <- data$ISO2[i] %>% tolower()
  list(
    source = paste0("https://hatscripts.github.io/circle-flags/flags/", country, ".svg"),
    xref = "x",
    yref = "y",
    xanchor = "center",
    yanchor = "middle",
    x = data$gdp[i],
    y = data$co2_per_capita[i],
    sizex = (sqrt(data$co2[i] / max(data$co2)) * maxi * 0.2 + maxi * 0.05)*0.5,
    sizey = (sqrt(data$co2[i] / max(data$co2)) * maxi * 0.2 + maxi * 0.05)*0.5,
    sizing = "contain",
    opacity = 0.8,
    layer = "above"
  )
})

p_plotly <- ggplotly(p, tooltip = "text")
p_plotly %>%
  layout(images = image_annotations)

p <- ggplotly(p, tooltip="text") %>% 
  layout(images = image_annotations, hoverlabel = list(align = "left")) 
p 

htmlwidgets::saveWidget(as_widget(p), "emissions.html")



# Example data frame
data <- data.frame(
  ISO3 = c("USA", "CAN", "DEU", "FRA", "GBR"),
  ISO2 = c("us", "ca", "de", "fr", "gb"),
  gdp = c(21000000, 1600000, 4200000, 2900000, 2900000),
  co2_per_capita = c(15, 16, 9, 6, 8),
  co2 = c(5000, 250, 400, 200, 300),
  population = c(330000000, 38000000, 82000000, 67000000, 67000000),
  year = c(2020, 2020, 2020, 2020, 2020)
)

# Create the ggplot2 scatter plot
p <- ggplot(data, aes(x = gdp, y = co2_per_capita, color = region, text = paste(
  "Country:", ISO3,
  "<br>GDP (m USD):", round(gdp, 0),
  "<br>Population (th):", round(population / 1e3, 0),
  "<br>GDP per capita", round(gdp / population, 0),
  "<br>CO2 emissions (m tons)", round(co2, 0),
  "<br>CO2 emissions per capita", round(co2_per_capita, 2)
))) +
  geom_point(aes(frame=year, size = co2, alpha = 0.75)) +
  scale_size_continuous(range = c(0.01, 50)) +
  labs(x = "GDP in millions", y = "Tons of CO2 emissions per capita", alpha = "", color = "", size = "") +
  scale_y_continuous(limits = c(0, 65)) +
  theme_minimal()

# Convert ggplot to plotly
p_plotly <- ggplotly(p, tooltip = "text")

# Create frames for each year
years <- unique(data$year)
frames <- lapply(years, function(year) {
  subset_data <- data %>% filter(year == year)
  image_annotations <- lapply(1:nrow(subset_data), function(i) {
    country <- subset_data$ISO2[i] %>% tolower()
    list(
      source = paste0("https://hatscripts.github.io/circle-flags/flags/", country, ".svg"),
      xref = "x",
      yref = "y",
      xanchor = "center",
      yanchor = "middle",
      x = subset_data$gdp[i],
      y = subset_data$co2_per_capita[i],
      sizex = (sqrt(subset_data$co2[i] / max(subset_data$co2)) * 0.2 + 0.05) * 0.5,
      sizey = (sqrt(subset_data$co2[i] / max(subset_data$co2)) * 0.2 + 0.05) * 0.5,
      sizing = "contain",
      opacity = 0.8,
      layer = "above"
    )
  })
  list(
    name = as.character(year),
    layout = list(images = image_annotations)
  )
})


p_plotly$x$layout$
  
  # Add frames to the plotly object
  p_plotly <- p_plotly %>%
  layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        x = 1.05,
        y = 0.5,
        buttons = lapply(years, function(year) {
          list(
            method = 'animate',
            args = list(
              list(as.character(year)),
              list(frame = list(duration = 1000, redraw = TRUE), fromcurrent = TRUE)
            ),
            label = as.character(year)
          )
        })
      )
    ),
    sliders = list(
      list(
        steps = lapply(years, function(year) {
          list(
            method = 'animate',
            args = list(
              list(as.character(year)),
              list(mode = 'immediate', frame = list(duration = 1000, redraw = TRUE), fromcurrent = TRUE)
            ),
            label = as.character(year)
          )
        }),
        active = 0
      )
    ),
    frames = frames
  ) %>%
  animation_opts(frame = 1000, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Year: "))

# Display the Plotly plot
p_plotly




