# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(here)
library(readr)
library(htmlTable)
library(tidyr)

dat <- read_csv(here("Data/Mens_Tennis_Grand_Slam_Winner.csv"))

# Define UI for application that shows Grand Slam results by year and tournament
ui <- fluidPage(

  # Application title
  titlePanel("Men's Tennis Grand Slam Results"),

  # Sidebar with choices for year between 1950-Present
  sidebarLayout(
    sidebarPanel(
      selectInput("year",
                  "Select Year:",
                  choices = unique(dat$YEAR)
      ), # selectInput
      selectInput("tournament",
                  "Select Grand Slam Tournament:",
                  choices = unique(dat$TOURNAMENT)
      ) # selectInput

    ), # sidebarPanel

    # Show a table of the selected data
    mainPanel(
      tabsetPanel(
        tabPanel("Winner", tableOutput("table")),
        tabPanel("Stats", plotOutput("statsPlot"))
      ),
      imageOutput("image")
    ) # mainPanel

  ) # sidebarLayout

) # fluidPage

# Define server logic required to draw the table
server <- function(input, output, session) {

  # Render the main table based on the selected year and tournament
  output$table <- renderTable({
    filtered_data <- dat %>%
      rename("DOMINANT HAND" = WINNER_LEFT_OR_RIGHT_HANDED,
             "WINNER PRIZE MONEY" = WINNER_PRIZE,
             "ATP RANKING" = WINNER_ATP_RANKING,
             "RUNNER-UP ATP RANKING" = `RUNNER-UP_ATP_RANKING`) %>%
      filter(YEAR == input$year & TOURNAMENT == input$tournament) %>%
      select(WINNER, "WINNER PRIZE MONEY", "ATP RANKING", "DOMINANT HAND", `RUNNER-UP`, "RUNNER-UP ATP RANKING")

    filtered_data
  }) # renderTable

  # Render the additional stats Plot with Prize money over the years
  output$statsPlot <- renderPlot({
    winner_data <- dat %>%
      filter(YEAR == input$year & TOURNAMENT == input$tournament) %>%
      select(TOURNAMENT, WINNER, `RUNNER-UP`, PRIZE_MONEY_WINNER)

    winner_data

  }) # renderTable

  output$image <- renderImage({
    if (input$tournament == "Wimbledon") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/Wimbledon_image.jpeg")
    } else if (input$tournament == "French Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/french_image.webp")
    } else if (input$tournament == "U.S. Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/US_image.jpeg")
    } else if (input$tournament == "Australian Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/austrailian_image.jpeg")
    } else {
      # Default image
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/ATP_image.jpeg")
    }

    list(src = filename, width = "80%", height = "auto")
  }, deleteFile = FALSE)

  # output$image <- renderImage({
  #     filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/STAT431/431Portfolio/Mens-Tennis-History/www/austrailian_image.jpeg")
  #
  #     list(src = filename)
  # }, deleteFile = FALSE)


} # server

# Run the application
shinyApp(ui = ui, server = server)
