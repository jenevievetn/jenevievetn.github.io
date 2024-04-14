library(ggplot2)
library(shiny)
library(tidyverse)

top10_emotion_avg_long <- read.csv("top10_emotion_avg_long.csv")

# Create a named vector mapping character names to custom colors
character_colors <- c("Harry Potter" = "#00C1A3",
                      "Ron Weasley" = "#F8766D",
                      "Hermione Granger" = "#C77CFF",
                      "Albus Dumbledore" = "#00B0F6",
                      "Rubeus Hagrid" = "#FF62BC",
                      "Severus Snape" = "#60B738",
                      "Minerva McGonagall" = "#D89000",
                      "Horace Slughorn" = "#9590FF",
                      "Voldemort" = "#A3A500",
                      "Neville Longbottom" = "#00BF7D")

# Define UI
ui <- fluidPage(
  titlePanel("Distribution of Emotion Sentiments by Character"),
  sidebarLayout(
    sidebarPanel(
      selectInput("emotion", "Select Emotion:", choices = unique(top10_emotion_avg_long$Emotion))
    ),
    mainPanel(
      plotOutput("lollipop_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$lollipop_plot <- renderPlot({
    # Subset the data for the selected emotion
    emotion_data <- top10_emotion_avg_long[top10_emotion_avg_long$Emotion == input$emotion, ]
    
    # Plot the lollipop graph
    p <- ggplot(emotion_data, aes(x = reorder(`Character Name`, `Average Sentiment`), y = `Average Sentiment`)) +
      geom_segment(aes(xend = `Character Name`, yend = 0, color = `Character Name`), size = 1) +
      geom_point(aes(color = `Character Name`), size = 4, alpha = 0.6) +
      labs(title = paste("Average", input$emotion, "Sentiments by Character"),
           x = "Characters",
           y = "Average Sentiment") +
      theme_minimal() +
      coord_flip() +
      scale_color_manual(values = character_colors) +  # Assign specific colors to character names
      theme(
        axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none", # Remove legend
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        legend.spacing.y = unit(0.1, "cm"))
    
    # Print the plot
    print(p)
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
#rsconnect::deployApp('C:/Users/Jen.ACER-JN/Documents/SKOOO/Y1 S2 Modules 2024/NM2207/Project/jenevievetn.github.io/app3.R')
