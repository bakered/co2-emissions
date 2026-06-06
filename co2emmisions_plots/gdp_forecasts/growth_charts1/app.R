# Install necessary libraries if not installed
# install.packages(c("shiny", "ggplot2", "plotly", "dplyr"))

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# Define the growth_rate_plot function
growth_rate_plot <- function(data, base_year = 2000, variable_type = "Real, 2015 $", development_status = "Developed economies", per_capita=TRUE) {
  
  if(variable_type=="Real, 2015 $"){
    variable_suffix = "_2015"
  } else {
    variable_suffix = ""
  }
  
  # If per_capita is TRUE, add '_pc' to the suffix for the columns
  if (per_capita) {
    variable_suffix = paste0(variable_suffix, "_pc")
  }
  
  # Prepare the data based on the base year and variable suffix
  data_long <- data %>%
    filter(year > base_year - 1) %>% 
    filter(country == development_status) %>% 
    mutate(
      GDP_value = get(paste0("GDP", variable_suffix)) * 100 / pull(filter(data, year == base_year & country == development_status), paste0("GDP", variable_suffix)),
      FDI_value = get(paste0("FDI", variable_suffix)) * 100 / pull(filter(data, year == base_year & country == development_status), paste0("FDI", variable_suffix)),
      trade_value = get(paste0("trade", variable_suffix)) * 100 / pull(filter(data, year == base_year & country == development_status), paste0("trade", variable_suffix))
    ) %>%
    gather(key = "growth_rate_type", value = "growth_rate_value", all_of(c("GDP_value", "FDI_value", "trade_value")))
  
  # Plot the growth rate
  p <- ggplot(data_long, aes(x = year, y = growth_rate_value, colour = growth_rate_type)) +
    geom_line(linewidth = 1.2) +  # Make lines thicker for better readability
    scale_color_brewer(palette = "Set1") +  # Color palette
    labs(
      title = paste("Growth of", development_status),
      x = NULL,
      y = NULL,
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
  
  # Use plotly for interactive plotting
  p_plotly <- ggplotly(p) %>%
    layout(
      legend = list(
        orientation = "h",  # Horizontal legend
        x = 0.5,            # Center horizontally
        xanchor = "center", # Align the center
        y = -0.2            # Place it below the plot
      )
    )
  
  return(p_plotly)
}

# Define UI
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      numericInput("base_year", "Base Year:", value = 2000, min = 1960, max = 2023, step = 1),
      selectInput("variable_type", "Variable Type:", choices = c("Real, 2015 $", "Nominal, Current $"), selected = "Real, 2015 $"),
      selectInput("country", "Country Group:", choices = c("Developed economies", "Developing economies", "World"), selected = "Developing economies"),
      checkboxInput("per_capita", "Per Capita", value = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("growth_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  #load data
  load("combined.RData")
  
  # Generate the plot when the button is clicked
  observe({
    output$growth_plot <- renderPlotly({
      growth_rate_plot(
        data = combined,
        base_year = input$base_year,
        variable_type = input$variable_type,
        development_status = input$country,
        per_capita = input$per_capita
      )
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
