library(shiny)
library(dplyr)
library(ggplot2)
#library(rsconnect)

load("loaded.Rdata")
source("function.R")
# source(paste0(folder, "load_data.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Low Budget COVID-19 Dash"),
    h4(paste0("Updated as of ", add_readable_time())),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectD", h5(strong("Select Data")),
                      choices = list("Domestic - States", 
                                     "Domestic - Counties",
                                     "International - Country", 
                                     "International - Province/Region"), 
                      selected = "Domestic - USA"),
            
            uiOutput("choose_location"),
            
            "COVID-19 New Cases Summary:",
            textOutput("text")
        
        ),
        mainPanel(
          plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$choose_location <- renderUI({
    if(is.null(input$selectD))
      return()
    
    if (input$selectD == "Domestic - States") {
      locations <- us_states
    } else if (input$selectD == "Domestic - Counties"){
      locations <- us_counties
      } else if (input$selectD == "International - Country"){
        locations <- country
        } else if (input$selectD == "International - Province/Region"){
          locations <- province[province != ""]
      }

    selectInput("choose_location", "Choose Location", 
                       choices  = sort(locations),
                       selected = sort(locations)[1])
  })
  output$text <- renderText({
    email_body_func(state_fun(input$choose_location))
    })
  output$plot <- renderPlot({
    mavg_plot_func(state_fun(input$choose_location))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
# rsconnect::deployApp(appFiles = c("app.R", "function.R", "loaded.Rdata"), appName = "master_shiny", launch.browser = F, forceUpdate = T)