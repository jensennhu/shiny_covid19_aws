library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
#library(rsconnect)

country <- readRDS("country.Rdata") 
province <- readRDS("province.Rdata")
us_states <- readRDS("us_states.Rdata") 
us_counties <- readRDS("us_counties.Rdata")
covid_state <- readRDS("covid_state.Rdata")
covid_county <- readRDS("covid_county.Rdata")
international <- readRDS("international.Rdata")

source("function.R")
# source(paste0(folder, "load_data.R"))

ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("The 'Rona Digest [beta.]"),
    h4(paste0("Updated as of ", Sys.Date())),

    # Sidebar 
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

# Define server logic required 
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