library(shiny)
library(shinythemes)
library(DT)

shinyUI(
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("Mathematics and Statistics Schedule"),
    
    fluidRow(
      column(3, 
             actionButton("refresh", "(Re)load data", class = "btn-primary"),
             textOutput("message")
      ),
      column(3,
             uiOutput("year1UI")
      ),
      column(3,
             h6("If you have sufficient permissions, you can visit ",
                a("this Google Sheet", href = ss$browser_url, target="_blank"), ".")
      )
    ),
    tabsetPanel(
      selected = "Faculty Schedules",
      tabPanel(
        "Faculty Schedules", 
        p("Click to see a listing of a faculty member's load."),
        plotOutput("fac_plot", click = "fac_click"),
        dataTableOutput("fac_details")
      ),
      tabPanel( 
        "Loads", 
        fluidRow(
          column(4,
                 checkboxInput("by_term", "Separate by term", value = FALSE),
                 dataTableOutput("loads")
          ),
          p("Click on rows of the table to the left to show faculty schedule details here."),
          column(8, dataTableOutput("fac_details2"))
        )
      ),
      tabPanel(
        "Room Use", 
        p("Brush a section of the schedule to see schedule details."),
        plotOutput("room_plot", hover = "room_hover", brush = "room_brush"),
        dataTableOutput("room_details")
        # verbatimTextOutput("stuff")
      ),
      tabPanel(
        "Compare Schedules", 
        fluidRow(
          column(3, uiOutput("year2UI")),
          column(3, uiOutput("columnsUI"))
        ),
        dataTableOutput("comparison_data")
      ), 
      tabPanel(
        "Schedule", 
        downloadButton("downloadSchedule", "Download as CSV"), 
        dataTableOutput("schedule")
      )
    )
  )
)