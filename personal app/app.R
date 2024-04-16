library(ggplot2)
library(shiny)
library(tidyverse)
library(plotly)

# Load the data
avg_emotion_per_chapter <- read.csv("avg_emotion_per_chapter.csv")
text_by_chapter <- read.csv("text_by_chapter.csv")

filtered_data <- text_by_chapter[!is.na(text_by_chapter$chapter), ]

# Custom column headers in avg_emotion_per_chapter
# Rename the columns
colnames(avg_emotion_per_chapter) <- 
  c("Book", "Chapter", "Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")

# Define custom emotion names
custom_emotion_names <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")

# Define custom hex colors for each book title
custom_colors <- c("#EB022E", "#008FD5", "#F9BC44", "#933289", "#FB924B", "#6A8E4B", "#8B8B8B", "#024B8F")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(
    div(
      style = "font-size: 23px;",  # Adjust font size here
      "Emotional Trends Across the Harry Potter Series"
    )
  ),
  
  # Add tabset panel for multiple tabs
  tabsetPanel(
    
    # First tab for sentiment analysis
    tabPanel("Sentiment Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting emotion
                 selectInput("emotion", "Select Emotion:",
                             choices = custom_emotion_names)),
               mainPanel(
                 plotlyOutput("sentiment_plot")
               )
             )),
    
    # Second tab for text summaries by chapter
    tabPanel("Chapter Summary",
             sidebarLayout(
               sidebarPanel(
                 # Text input for chapter number
                 textInput("chapter_number", "Enter chapter number seen on graph:")),
               mainPanel(
                 # Output box for book title
                 tags$div(
                   h4("Book Title"),
                   verbatimTextOutput("book_title_output")
                 ),
                 # Output box for book chapter
                 tags$div(
                   h4("Book Chapter"),
                   verbatimTextOutput("chapter_output")
                 ),
                 # Output box for summary
                 tags$div(
                   h4("Summary"),
                   verbatimTextOutput("summary_output")
                 )
               )
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render the sentiment plot
  output$sentiment_plot <- renderPlotly({
    
    # Get the selected emotion
    emotion <- input$emotion
    
    # Plot sentiment chart for the selected emotion
    plot <- ggplot(avg_emotion_per_chapter, aes(x = Chapter, y = !!sym(emotion), color = Book)) +
      geom_line(linewidth = 0.5) + # Increase the weight of the line
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
        legend.text = element_text(size = 6), # Change the font size of legend text
        legend.title = element_text(size = 7), # Change the font size of legend title
        legend.spacing.y = unit(0.1, "cm")      # Adjust legend item spacing
      ) +
      scale_color_manual(values = custom_colors)  # Set custom hex colors for book titles
    
    # Convert ggplot to plotly object
    ggplotly(plot)
  })
  
  # Render book title
  output$book_title_output <- renderText({
  # Get the selected chapter number
  chapter_number <- input$chapter_number
  
  if (!is.na(chapter_number) && chapter_number != "") {
    # Convert chapter_number to numeric format
    chapter_number <- as.integer(chapter_number)
    # Filter the text_by_chapter dataset for the selected chapter
    filtered_text <- filtered_data[filtered_data$chapter == chapter_number, c("book_title", "chapter_name", "summary")]
    
    # Return the corresponding book title
    if (nrow(filtered_text) > 0) {
      filtered_text$book_title
    } else {
      "Chapter not found."
    }
  } else {
    # If no input is provided, display a default message
    "Give an input."
  }
})

# Render book chapter 
output$chapter_output <- renderText({
  input$chapter_number
  
  # Get the selected chapter number
  chapter_number <- input$chapter_number
  
  if (!is.na(chapter_number) && chapter_number != "") {
    # Convert chapter_number to numeric format
    chapter_number <- as.integer(chapter_number)
    # Filter the text_by_chapter dataset for the selected chapter
    filtered_text <- filtered_data[filtered_data$chapter == chapter_number, c("book_title", "chapter_name", "summary")]
    # Return the corresponding summary
    if (nrow(filtered_text) > 0) {
      filtered_text$chapter
    } else {
      "Chapter not found."
    }
  } else {
    # If no input is provided, display a default message
    "Give an input."
  }
})

# Render text summary for the selected chapter
output$summary_output <- renderText({
  # Get the selected chapter number
  chapter_number <- input$chapter_number
  
  if (!is.na(chapter_number) && chapter_number != "") {
    # Convert chapter_number to numeric format
    chapter_number <- as.integer(chapter_number)
    # Filter the text_by_chapter dataset for the selected chapter
    filtered_text <- filtered_data[filtered_data$chapter == chapter_number, c("book_title", "chapter_name", "summary")]
    # Return the corresponding summary
    if (nrow(filtered_text) > 0) {
      filtered_text$summary
    } else {
      "Chapter not found."
    }
  } else {
    # If no input is provided, display a default message
    "Give an input."
  }
})
  
}

# Run the application
shinyApp(ui = ui, server = server)

