library(shiny)
library(shinythemes)

shinyUI(
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("Mathematics and Statistics Schedule"),
    
    fluidRow(
      column(3,
             h6("If you have sufficient permissions, you can visit ",
                a("this Google Sheet", href = ss$browser_url, target="_blank"), ".")
      ),
      column(3,
             uiOutput("year1UI")
      ),
      column(3, 
             actionButton("refresh", "Refresh data", class = "btn-primary"),
             textOutput("message")
      )
    ),
    tabsetPanel(
      tabPanel(
        "Schedule", 
        downloadButton("downloadSchedule", "Download as CSV"), 
        dataTableOutput("schedule")
      ),
      tabPanel(
        "Loads", 
        dataTableOutput("loads")
      ),
      tabPanel(
        "Room Use", 
        p("Brush a section of the schedule to see schedule details."),
        plotOutput("room_plot", hover = "room_hover", brush = "room_brush"),
        dataTableOutput("room_details")
        # verbatimTextOutput("stuff")
      ),
      tabPanel(
        "Faculty Schedules", 
        p("Click to see a listing of a faculty member's load."),
        plotOutput("fac_plot", click = "fac_click"),
        dataTableOutput("fac_details")
      ),
      tabPanel(
        "Compare Schedules", 
        fluidRow(
          column(3, uiOutput("year2UI")),
          column(3, br(), br(), checkboxInput("full_comparison", "Show more details", value = FALSE))
        ),
        dataTableOutput("comparison_data")
      )
    )
  )
  
)