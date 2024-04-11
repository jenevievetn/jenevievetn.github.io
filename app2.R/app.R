library(ggplot2)
library(shiny)
library(tidyverse)

# Load the data
combined_sentiment <- read.csv("combined_sentiment.csv")

# Define custom hex colors for emotion
custom_colors <- c("#EB022E", "#008FD5", "#F9BC44", "#933289", "#FB924B", "#6A8E4B", "#8B8B8B", "#024B8F")

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(
    div(
      style = "font-size: 23px;",  # Adjust font size here
      "Combined Emotional Sentiments of the Harry Potter Series"
    )
    ),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Checkbox group for selecting emotions
      checkboxGroupInput("emotions", "Select Emotions:", 
                         choices = unique(combined_sentiment$emotion), 
                         selected = unique(combined_sentiment$emotion),
                         inline = TRUE)  # Display checkboxes inline
    ),
    # Main panel for displaying outputs
    mainPanel(
      # Plot output for sentiment chart
      plotOutput("sentiment_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Render the sentiment plot
  output$sentiment_plot <- renderPlot({
    # Filter data based on selected emotions
    filtered_sentiment <- combined_sentiment %>%
      filter(emotion %in% input$emotions)
    # Plot sentiment chart for the selected emotions
    ggplot(filtered_sentiment, aes(x = chapter, y = sentiment, color = emotion)) +
      geom_line(linewidth = 1) +
      labs(title = paste0(emotion, " Sentiment Analysis"),
           x = "Chapter",
           y = "Average Sentiment",
           colour = NULL) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "right",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.spacing.y = unit(0.1, "cm")
      ) +
      scale_color_manual(values = custom_colors)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# STEP 1 – INSTALL RSCONNECT
#install.packages('rsconnect')

# STEP 2 – AUTHORIZE ACCOUNT
#rsconnect::setAccountInfo(name='jenevievetn',
#                          token='1F55D0BF747C0A8BFB032D840C3C72E8',
#                          secret='ALSFWGD0EaoNnwMbhpqPzVqgfa+VBGBLhY/h6Dmr')

# STEP 3 – DEPLOY
# library(rsconnect)
# rsconnect::deployApp('C:/Users/Jen.ACER-JN/Documents/SKOOO/Y1 S2 Modules 2024/NM2207/Project/jenevievetn.github.io/app2.R')
