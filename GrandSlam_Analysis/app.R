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
library(ggplot2)
library(plotly)
library(RColorBrewer)

dat <- read_csv(here("Data/grandslam_prize_usd.csv"))

# Define UI for application that shows Grand Slam results by year and tournament
ui <- fluidPage(

  # Application title
  titlePanel("Tennis Grand Slam Results"),

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
      ), # selectInput

      actionButton("analysis", "Show Plot Analysis")

    ), # sidebarPanel


    # Show a table of the selected data
    mainPanel(
      tabsetPanel(
        tabPanel("Results", tableOutput("table1")),
        tabPanel("Tournament Prize Money Over Time", plotlyOutput("plot1"), htmlOutput("analysisText")),
        tabPanel("Average Prize Money Over Time", plotlyOutput("plot2"))
      ),

      imageOutput("image")

    ) # mainPanel

  ) # sidebarLayout

) # fluidPage

# Define server logic required to draw the table
server <- function(input, output, session) {

  # Render the main table based on the selected year and tournament
  output$table1 <- renderTable({
    filtered_data <- dat %>%
      rename("WINNER PRIZE MONEY" = WINNER_PRIZE,
             "WINNER NATIONALITY" = WINNER_NATIONALITY,
             "DIVISION" = GENDER) %>%
      mutate(DIVISION = ifelse(DIVISION == "Male", "Men's Singles", "Women's Singles")) %>%
      filter(YEAR == input$year & TOURNAMENT == input$tournament) %>%
      select(DIVISION, WINNER, "WINNER NATIONALITY", `RUNNER-UP`, "WINNER PRIZE MONEY")

    filtered_data
  }) # renderTable

  # Render the additional stats Plot with Prize money over the years
  output$plot1 <- renderPlotly({

    if (input$tournament == "Wimbledon"){
      pallete = c("#085716","#6836A5")
    }
    else if (input$tournament == "French Open") {
      pallete = c("#06492d", "#b06835")
    }
    else if (input$tournament == "U.S. Open"){
      pallete = c("#6C935C", "#3C638E")
    }
    else if (input$tournament == "Australian Open"){
      pallete = c("black", "#0073cf")
    }

    plot_data <- dat %>%
      rename("DIVISION" = GENDER) %>%
      mutate(DIVISION = ifelse(DIVISION == "Male", "Men's Singles", "Women's Singles")) %>%
      filter(TOURNAMENT == input$tournament) %>%
      plot_ly(x = ~YEAR, y = ~WINNER_PRIZE, color = ~DIVISION,
              colors = pallete) %>%
      add_lines() %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        title = paste("")
      )

    plot_data

  }) # renderTable

  output$plot2 <- renderPlotly

  output$image <- renderImage({
    if (input$tournament == "Wimbledon") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/Wimbledon_image.jpeg")
      surface_type <- "Surface Type: Grass"
    } else if (input$tournament == "French Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/french_image.webp")
      surface_type <- "Surface Type: Red Clay"
    } else if (input$tournament == "U.S. Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/US_image.jpeg")
      surface_type <- "Surface Type: Hard"
    } else if (input$tournament == "Australian Open") {
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/austrailian_image.jpeg")
      surface_type <- "Surface Type: Hard"
    } else {
      # Default image
      filename <- normalizePath("/Users/shreya.ravilla248/Desktop/R Projects/Tennis Grand Slam Analysis/www/ATP_image.jpeg")
    }

    list(src = filename, width = "90%", height = "auto")
  }, deleteFile = FALSE)


  observeEvent(input$analysis, {
    tournament <- input$tournament

      output$analysisText <- renderUI({
        if (tournament == "Australian Open"){
          HTML("<i>HTML Text for Australian Open</i>")
        } else if (tournament == "French Open"){
          HTML("<i>HTML Text for French Open</i>")
        } else if (tournament == "Wimbledon"){
          HTML("<i>HTML Text for Wimbledon</i>")
        } else if (tournament == "U.S. Open"){
          HTML("<i>HTML Text for U.S. Open</i>")
        }
      })

  }) # observeEvent

} # server

# Run the application
shinyApp(ui = ui, server = server)
