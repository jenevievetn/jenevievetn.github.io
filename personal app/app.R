library(ggplot2)
library(shiny)
library(tidyverse)

# Load the data
avg_emotion_per_chapter <- read.csv("avg_emotion_per_chapter.csv")


# Define custom emotion names
custom_emotion_names <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")

# Define custom hex colors for each emotion
custom_colors <- c("#EB022E", "#008FD5", "#F9BC44", "#933289", "#FB924B", "#6A8E4B", "#8B8B8B", "#024B8F")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(
    div(
      style = "font-size: 24px;",  # Adjust font size here
      "Emotional Sentiment of the Harry Potter Series"
    )
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Dropdown menu for selecting emotion
      selectInput("emotion", "Select Emotion:",
                  choices = custom_emotion_names)),
    
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
    
    # Get the selected emotion
    emotion <- input$emotion
    
    # Plot sentiment chart for the selected emotion
    plot <- ggplot(avg_emotion_per_chapter, aes(x = chapter, y = !!sym(paste0("avg_", tolower(emotion), "_sentiment")), color = book_title)) +
      geom_line(linewidth = 1) + # Increase the weight of the line
      labs(title = paste0(emotion, " Sentiment Analysis"),
           x = "Chapter",
           y = "Average Sentiment",
           color = NULL) +
      theme_minimal() + 
      theme(
        axis.text = element_text(size = 7),  # Decrease the font size of axis labels
        axis.title = element_text(size = 8),  # Change the font size of axis titles
        plot.title = element_text(size = 12),  # Change the font size of the plot title
        panel.background = element_rect(fill = "grey95",  # Make the entire panel grey
                                        colour = "grey95"),
        
        panel.grid.major = element_line(colour = "grey"),  # Make the major grid lines shaded grey
        legend.position = "bottom",  # Move legend to the bottom
        legend.text = element_text(size = 5), # Change the font size of legend text
        legend.title = element_text(size = 7), # Change the font size of legend title
        legend.spacing.y = unit(0.1, "cm")      # Adjust legend item spacing
      ) +
      scale_color_manual(values = custom_colors)  # Set custom hex colors for book titles
    
    # Print the plot
    print(plot)
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
#library(rsconnect)
#rsconnect::deployApp('C:/Users/Jen.ACER-JN/Documents/SKOOO/Y1 S2 Modules 2024/NM2207/Project/jenevievetn.github.io/personal app')
