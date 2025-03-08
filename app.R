library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(rsconnect)

# Read in the salary data
data <- read_xlsx("salary_data.xlsx") %>% 
  select(-notes) %>% 
  mutate(rating = as.integer(round(rating)))

# Convert categorical variables
data$level <- as.factor(data$level)

# Segment the data (for bonus graph)
data_without_na_ratings <- data %>% 
  filter(!is.na(rating))

# Segment the data (for raise graph)
data_without_na_raises <- data %>% 
  filter(!is.na(percent_raise))

# Read in inflation data (for percent raise graph)
inflation <- read.csv("inflation_data.csv") %>%
  filter(year >= min(data_without_na_raises$year))

# Train models
model_salary <- lm(base_pay ~ year + level, data = data)
model_bonus <- lm(bonus ~ level + rating, data = data)

# Function to make predictions
predict_salary <- function(year, level) {
  input_data <- data.frame(
    year = as.numeric(year),
    level = factor(level, levels = levels(data$level))
  )
  predict(model_salary, newdata = input_data)
}

predict_bonus <- function(level, rating) {
  input_data <- data.frame(
    level = factor(level, levels = levels(data$level)),
    rating = as.integer(rating)
  )
  predict(model_bonus, newdata = input_data)
}

# Define Server
server <- function(input, output) {
  
  observeEvent(input$predict, {
    salary_pred <- predict_salary(input$year, input$level)
    bonus_pred <- predict_bonus(input$level, input$rating)
    
    output$salary_prediction <- renderText({
      formatted_salary <- scales::comma(salary_pred, accuracy = 0.01)
      paste("Estimated Base Pay: $", formatted_salary)
    })
    
    output$bonus_prediction <- renderText({
      formatted_bonus <- scales::comma(bonus_pred, accuracy = 0.01)
      paste("Estimated Bonus: $", formatted_bonus)
    })
  })
  
  # Salary Trend Line Graph
  output$salary_trend_plot <- renderPlot({
    ggplot(data, aes(x = year, y = base_pay, color = level)) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      labs(
        x = "Year",
        y = "Annual Base Pay ($)",
        color = "Level"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines")
      )
  })
  
  # Bonus Box Plot
  output$bonus_vs_rating_plot <- renderPlot({
    ggplot(data_without_na_ratings,
           aes(x = factor(rating, levels = 1:5), y = bonus, fill = level)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.6) + 
      geom_count(aes(color = level, size = after_stat(n)), alpha = 0.8) +
      scale_size_continuous(range = c(2, 8)) +
      facet_wrap(~ level, scales = "free_y") +
      labs(
        x = "Rating",
        y = "Bonus ($)",
        fill = "Level",
        size = "Frequency"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines")
      )
  })
  
  # Percent Raise vs. Year with Inflation Line
  output$percent_raise_plot <- renderPlot({
    
    ggplot(data_without_na_raises,
           aes(x = year, y = percent_raise, color = level)) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      geom_line(data = inflation,
                aes(x = year, y = inflation_percent), 
                color = "black", linetype = "dotted", size = 1.2) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      labs(
        x = "Year",
        y = "Percent Raise (%)",
        color = "Level"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines")
      ) +
      scale_linetype_manual(
        name = "Legend",
        values = c("Level" = "solid", "Inflation" = "dotted"),
        labels = c("Level", "Inflation")
      )
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Guidehouse Salary and Bonus Prediction Model"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Enter Year:", value = 2025, min = 2000, max = 2035),
      selectInput("level", "Select Level:", choices = levels(data$level)),
      
      h4("Bonus Prediction"),
      selectInput("rating", "Select Rating:", choices = 1:5),
      
      actionButton("predict", "Predict Salary & Bonus")
    ),
    
    mainPanel(
      
      p(em("Developer's Note: This app is an independent project and is not affiliated
         with Guidehouse. It was created for educational purposes and as an experiment
         in building an R Shiny app with a real-world use case. While the data
         visualizations are interesting, this tool is not intended for decision-making,
         performance evaluations, or formal use - just exploration and learning!")),
      
      h3("Predicted Salary and Bonus:"),
      
      div(
        style = "font-size: 20px; font-weight: bold; color: #2E3B4E; 
                 background-color: #f8f9fa; padding: 10px; 
                 border-radius: 10px; text-align: center; 
                 border: 2px solid #2E3B4E; max-width: 400px;",
        verbatimTextOutput("salary_prediction"),
        verbatimTextOutput("bonus_prediction")
      ),
      
      hr(),
      
      # First Graph: Salary Trends Over Time
      h3("Salary Trends Over Time"),
      p("The following graph shows base salaries across different years, grouped by level."),
      plotOutput("salary_trend_plot"),
      
      hr(),
      
      # Second Graph: Bonus vs. Rating
      h3("Bonus Distribution by Rating and Level"),
      p("The regression analysis suggests that year is not a statistically significant factor in determining bonuses.
        Instead, level and rating have a much stronger effect."),
      p("However, there are some outliers where this trend does not hold, such as in 2024."),
      
      plotOutput("bonus_vs_rating_plot"),
      
      hr(),
      
      # Third Graph: Percent Raise vs. Year
      h3("Annual Percent Raise vs. Inflation"),
      p("This graph compares annual percent raises by level against the U.S. inflation rate."),
      p("The dotted black line represents inflation. Ideally, percent raises should outpace inflation for real income growth."),
      p("Note that large outlier raises indicate an individual promotion."),
      p("Raises are correlated to compa ratio. This study will be analyzed when there is more data."),
      
      plotOutput("percent_raise_plot")
    )
  )
)

# Run the app
shinyApp(ui, server)
