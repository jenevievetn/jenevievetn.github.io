library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput("samplesize","Sample Size:",min = 100,max = 10000,value = 1000)),
    mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$samplesize),col='darkorchid',xlab="Sample",main="Standard Normally Distributed Sample")},
    height=300
  )
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
