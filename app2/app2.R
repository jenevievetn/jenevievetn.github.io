library(ggplot2)
library(shiny)
library(tidyverse)
library(plotly)

# Load the data
combined_sentiment <- read.csv("combined_sentiment.csv")
text_by_chapter <- read.csv("text_by_chapter.csv")

# Custom column headers in avg_emotion_per_chapter
# Rename the columns
colnames(combined_sentiment) <- 
  c("Book", "Chapter", "Emotion", "Sentiment")

filtered_data <- text_by_chapter[!is.na(text_by_chapter$chapter), ]

# Define custom hex colors for emotion
custom_colors <- c("#EB022E", "#008FD5", "#F9BC44", "#933289", "#FB924B", "#6A8E4B", "#8B8B8B", "#024B8F")

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(
    div(
      style = "font-size: 23px;",  # Adjust font size here
      "Compare Emotion Sentiments Across the Series"
    )
  ),
  
  # Add tabset panel for multiple tabs
  tabsetPanel(
    
    # First tab for sentiment analysis
    tabPanel("Sentiment Analysis",
             sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        # Checkbox group for selecting emotions
        checkboxGroupInput("emotions", "Select Emotions:", 
                         choices = unique(combined_sentiment$Emotion), 
                         selected = unique(combined_sentiment$Emotion),
                         inline = TRUE)  # Display checkboxes inline
    ),
    
        # Main panel for displaying outputs
        mainPanel(
          # Plot output for sentiment chart
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
           ))))

# Define server logic
server <- function(input, output, session) {
  
  # Render the sentiment plot
  output$sentiment_plot <- renderPlotly({
    # Filter data based on selected emotions
    filtered_sentiment <- combined_sentiment %>%
      filter(Emotion %in% input$emotions)
    # Plot sentiment chart for the selected emotions
    plot <- ggplot(filtered_sentiment, aes(x = Chapter, y = Sentiment, color = Emotion)) +
      geom_line(linewidth = 0.5) +
      labs(title = " Sentiment Analysis",
           x = "Chapter",
           y = "Average Sentiment",
           colour = NULL) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "right",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.spacing.y = unit(0.1, "cm")
      ) +
      scale_color_manual(values = custom_colors)
    
    # Convert ggplot to plotly object
    ggplotly(plot)
  })
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

# Render book title


# Run the application
shinyApp(ui = ui, server = server)

