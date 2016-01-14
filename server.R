library(shiny)
library(DT)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

options(DT.options = 
          list(pageLength = 25, pagemenu = c(25, 50, 100, 200), 
               filter = list(position = 'top', clear = FALSE)
               )
        ) 

shinyServer(function(input, output, session) {
  
#   observeEvent(
#     input$reset,
#     gs_edit_cells(ss, input = filler)
#   )
#   observeEvent(
#     input$submit,
#     gs_edit_cells(ss, input = input$contents,
#                   ## the +1 business is to avoid writing into the header row
#                   anchor = cell_limits(c(input$row + 1, input$column),
#                                        c(input$row + 1, input$column)))
#   )
  
  rv <- reactiveValues( 
    SS = googlesheets::gs_title("MathStatSchedules"),
    foo = rnorm(1) 
 )
  
 query <- reactive( 
   parseQueryString(session$clientData$url_search) 
 )
  
  
  year1 <- reactive({ as.character(input$year1)[1] })
  year2 <- reactive({ as.character(input$year2)[1] })
  
  the_message <-
    eventReactive( 
      {input$refresh; rv$SS},
      paste("Last Modification of loaded spreadsheet: ", gs_gs(rv$SS)$updated)
    )
  
  room_cap <-
    eventReactive( 
      {input$refresh; rv$SS},
      {
        room_info <- gs_read(rv$SS, "Rooms")
        x <- room_info$Capacity
        names(x) <- room_info$Room
        x
      }
    )
  
  output$message <- renderText( the_message() )
  # output$message <- renderText({ query()[["admin"]] })
 
  observeEvent( 
    {input$refresh; rv$foo}, 
    rv$SS <- googlesheets::gs_title("MathStatSchedules")
  ) 
  
  the_sched <- 
    eventReactive({input$refresh; year1()}, 
                  gs_read(rv$SS, year1()) %>%
                    mutate(
                      ClassMax = ifelse(is.na(ClassMax), room_cap()[Room], ClassMax),
                      MeetingDays = sub("TH", "R", MeetingDays),
                      Term = gsub("\\d", "", Term),
                      Term = gsub("/", "", Term)
                    ) %>%
                    group_by(Term, SubjectCode, CourseNum) %>%
                    arrange(char2Time(MeetingStart)) %>%
                    mutate(
                      Section = 
                        ifelse(
                          is.na(Section),
                          setdiff(LETTERS, Section)[rank(Section, na.last = FALSE)],
                          Section
                        )
                    ) %>% 
                    ungroup() %>%
                    arrange(Term, SubjectCode, CourseNum, Section),
                  ignoreNULL = FALSE)
  
  the_sched2 <- 
    eventReactive({input$refresh; year2()}, 
                  gs_read(rv$SS, year2()) %>%
                    mutate(
                      ClassMax = ifelse(is.na(ClassMax), room_cap()[Room], ClassMax),
                      MeetingDays = sub("TH", "R", MeetingDays),
                      Term = gsub("\\d", "", Term),
                      Term = gsub("/", "", Term)
                    ) %>%
                    group_by(Term, SubjectCode, CourseNum) %>%
                    arrange(char2Time(MeetingStart)) %>%
                    mutate(
                      Section = 
                        ifelse(
                          is.na(Section),
                          setdiff(LETTERS, Section)[rank(Section, na.last = FALSE)],
                          Section
                        )
                    ) %>% 
                    ungroup() %>%
                    arrange(Term, SubjectCode, CourseNum, Section),
                  ignoreNULL = FALSE)
  
  the_sched_by_day <-
    reactive({
        rbind(
          the_sched() %>% filter(grepl("M", MeetingDays)) %>% mutate(Day="M"),
          the_sched() %>% filter(grepl("T", MeetingDays)) %>% mutate(Day="T"),
          the_sched() %>% filter(grepl("W", MeetingDays)) %>% mutate(Day="W"),
          the_sched() %>% filter(grepl("R", MeetingDays)) %>% mutate(Day="R"),
          the_sched() %>% filter(grepl("F", MeetingDays)) %>% mutate(Day="F")
        ) %>% 
        mutate(Day = factor(Day, levels=c("M","T","W","R","F"))) %>%
        filter(!is.na(Room)) %>%
        mutate(
          CourseLevel = as.character(courseLevel(CourseNum)),
          nDayLo = as.numeric(Day) - 0.3,
          nDayHi = as.numeric(Day) + 0.3,
          startTime = char2Time(MeetingStart),
          endTime = char2Time(MeetingEnd)
        )
    })
  
  the_duties <- eventReactive({input$refresh; rv$foo},
                             gs_read(rv$SS, "Duties"), ignoreNULL = FALSE)
  the_fac <- eventReactive({input$refresh; rv$foo},
                              gs_read(rv$SS, "Faculty"), ignoreNULL = FALSE)
  the_data <- reactive({merge(the_sched(), the_fac(), all.x = TRUE, all.y = FALSE)})
  
  fac_loads <- reactive({
    sked <- 
      the_sched() %>% 
      merge(the_duties(), all.x = TRUE, all.y = FALSE, by = "InstrMethod") %>%
      mutate(Teaching = ifelse(is.na(Teaching), "Other", Teaching)) %>%
      group_by(Faculty, Teaching)
    if (input$by_term) 
      sked <- sked %>% group_by(Term, add = TRUE)
    sked <- 
      sked %>% 
      summarise(Load = sum(FacultyLoad, na.rm = TRUE)) %>%
      ungroup() 
    sked <- 
      sked %>%
      spread(Teaching, Load) 
    sked <- 
      sked %>%
      mutate(Total = ifelse(is.na(Teaching), 0, Teaching) + 
                     ifelse(is.na(`Non-Teaching`), 0, `Non-Teaching`))
    sked
  })
  
  output$loads <-
    renderDataTable( fac_loads() )
  
  output$schedule <- 
    renderDataTable( DT::datatable(the_sched(), filter = list(position = "top")) )
      # %>%  select_(.dots = intersect(names(the_data()), viewCols)))
  
  output$downloadSchedule <-
    downloadHandler(
      filename = function() {
        paste0("MathStatSched-", year1(), "-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(the_data(),file, row.names = FALSE)
      }
    )
  
               
  observeEvent(
    { rv$SS; input$refresh},
    {
      output$year1UI <-
        renderUI(
          selectInput(
            "year1", "Primary Schedule", 
            choices = grep("^\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE)
          )
        )
    }
  )
  
  observeEvent(
    { year1(); rv$SS; input$refresh},
    {
      output$year2UI <-
        renderUI(
          selectInput(
            "year2", "Secondary Schedule", 
            choices = 
              ifelse(!is.null(query()[["admin"]]) && query()[["admin"]] == "rpruim",
                     setdiff(
                       grep("\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE),
                       year1()
                     ),
                     setdiff(
                       grep("^\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE),
                       year1()
                     )
              )
          )
        )
    }
  )
  
 output$room_plot <- 
   renderPlot({
     ggplot(the_sched_by_day()) + 
       geom_point(aes(x = Day, y = startTime), colour = "transparent") +
       geom_rect(
         aes(xmin = nDayLo,
             xmax = nDayHi,
             ymin = startTime,
             ymax = endTime,
             fill = CourseLevel
             ),
         alpha = 0.5
       ) +
       facet_grid( Term ~ Room) +
       labs(y = "Time")
   })
 
 output$fac_plot <- 
   renderPlot({
     ggplot(the_sched_by_day()) + #  %>% mutate(Fac = paste0(Last, ", ", substr(First, 1, 1)))) + 
       geom_point(aes(x = Day, y = startTime), colour = "transparent") +
       geom_rect(
         aes(xmin = nDayLo,
             xmax = nDayHi,
             ymin = startTime,
             ymax = endTime,
             fill = CourseLevel
         ),
         alpha = 0.5
       ) +
       facet_grid(Term ~ Faculty) +
       labs(y = "Time")
   })
 

   output$room_details <- 
     renderDataTable({
       if (!is.null(input$room_brush)) {
       the_sched_by_day() %>%
         filter(
           char2Time(MeetingEnd) >= input$room_brush$ymin,
           char2Time(MeetingStart) <= input$room_brush$ymax,
           as.numeric(Day) %>% between(input$room_brush$xmin - 0.3, input$room_brush$xmax + 0.3),
           Room == input$room_brush$panelvar1,
           Term == input$room_brush$panelvar2
         ) %>% 
           select(Term, SubjectCode, CourseNum, Section, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, ClassMax, Faculty, FacultyLoad) %>%
           unique()
       } else {
         the_sched() %>% 
           select(Term, SubjectCode, CourseNum, Section, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, ClassMax, Faculty, FacultyLoad) %>% 
           filter(FALSE)
       }
       # brushedPoints(the_sched_by_day(), input$room_brush) 
     })
 
   
   output$fac_details <- 
     renderDataTable({
       if (is.null(input$fac_click)) {
         the_sched() %>% filter(FALSE)
       } else {
         the_sched() %>%
           filter(Faculty == input$fac_click$panelvar1) %>% 
           select(Faculty, Term, SubjectCode, CourseNum, Section, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, ClassMax, FacultyLoad) 
       }
     })
   
   output$columnsUI <- 
     renderUI({
       checkboxGroupInput(
         "comp_columns", "include", 
         choices = 
           c("Term", "SubjectCode", "CourseNum", "MeetingDays", 
                     "MeetingStart", "MeetingEnd", "Room", "ClassMax", "Faculty", 
                     "FacultyLoad", "InstrMethod"),
          selected = 
           c("Term", "SubjectCode", "CourseNum", 
             # "MeetingDays",  "MeetingStart", "MeetingEnd", "Room", "ClassMax", "Faculty", 
               "FacultyLoad", "InstrMethod")
       )                        
     })
   
   output$comparison_data <- 
     renderDataTable({
       DT::datatable(
         filter = list(position = "top"),
         bind_rows(
           the_sched() %>% 
             select_(.dots = intersect(names(the_sched()), input$comp_columns)) %>% 
             anti_join(the_sched2() %>% 
                         select_(.dots = intersect(names(the_sched2()), input$comp_columns))) %>% 
             mutate(Schedule = "Primary"),
           the_sched2() %>% 
             select_(.dots = intersect(names(the_sched2()), input$comp_columns)) %>% 
             anti_join(the_sched() %>% 
                         select_(.dots = intersect(names(the_sched()), input$comp_columns))) %>% 
             mutate(Schedule = "Secondary") 
             ) %>%
           select_(.dots = c("Schedule", intersect(names(the_sched()), input$comp_columns))) %>%
           group_by_(.dots = names(.)) %>%
           summarise(n = n()),
         options = list(rowCallback = JS(
           'function(row, data) {
              if (data[1] == "Primary")
                $("td", row).css("color", "green");  
              else if (data[1] == "Secondary")
                $("td", row).css("color", "red");
           }'
         ))
       )
     })
})