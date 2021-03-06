library(shiny)
library(DT)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(mosaic)
library(hms)  # for converting times coming out of google sheet
# library(readxl)

source("global.R")


options(DT.options = 
          list(pageLength = 100, pagemenu = c(25, 50, 100, 200), 
               filter = list(position = 'top', clear = FALSE)
               )
        ) 

# # read data from Registrar's office for cross checking
# F16 <- readxl::read_excel("16fa math.xlsx")
# F16 <- F16[(1:nrow(F16)/2) * 2 - 1, ]


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
  
  # output$stuff <- renderPrint(glimpse(the_sched() %>% mutate(T = class(char2Time(MeetingStart))[1])))
  # output$stuff <- renderPrint(char2Time)
  
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
      if (input$refresh < 1) {
        "No data displayed?  Try hitting the (Re)load button"
      } else {
        paste("Last Modification of loaded spreadsheet: ", gs_gs(rv$SS)$updated)
      }
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
                      RoomMax = room_cap()[Room],
                      GlobalMax = ifelse(is.na(GlobalMax), 45, GlobalMax),
                      MeetingDays = sub("TH", "R", MeetingDays),
                      MeetingStart = ifelse(is.na(MeetingStart), "", as.character(MeetingStart) %>% sub(":00$", "", .)),
                      MeetingEnd = ifelse(is.na(MeetingEnd), "", as.character(MeetingEnd) %>% sub(":00$", "", .)),
                      Term = gsub("\\d", "", Term),
                      Term = gsub("/", "", Term)
                    ) %>%
                    filter(! grepl("Hidden", InstructionalMethod)) %>%
                    group_by(Term, SubjectCode, CourseNum) %>%
                    arrange(char2Time(MeetingStart)) %>%
                    mutate(
                      SectionCode =
                        ifelse(
                          is.na(SectionCode) & InstructionalMethod %in% c("Lecture", "Seminar", "Practicum"),
                          setdiff(LETTERS, SectionCode)[rank(SectionCode, na.last = FALSE)],
                          SectionCode
                        )
                    ) %>%
                    ungroup() %>%
                    arrange(Term, SubjectCode, CourseNum, SectionCode) %>%
                    left_join(gs_read(rv$SS, "Faculty")),
                  ignoreNULL = FALSE) 
  
 #  %>% select(viewCols)
  
  the_sched2 <- 
    eventReactive({input$refresh; year2()}, 
                  gs_read(rv$SS, year2()) %>%
                    mutate(
                      RoomMax = room_cap()[Room],
                      GlobalMax = ifelse(is.na(GlobalMax), 45, GlobalMax),
                      MeetingDays = sub("TH", "R", MeetingDays),
                      MeetingStart = ifelse(is.na(MeetingStart), "", as.character(MeetingStart) %>% sub(":00$", "", .)),
                      MeetingEnd = ifelse(is.na(MeetingEnd), "", as.character(MeetingEnd) %>% sub(":00$", "", .)),
                      Term = gsub("\\d", "", Term),
                      Term = gsub("/", "", Term),
                      CourseNum = as.character(CourseNum)
                    ) %>%
                    group_by(Term, SubjectCode, CourseNum) %>%
                    arrange(char2Time(MeetingStart)) %>%
                    mutate(
                      SectionCode =
                        ifelse(
                          is.na(SectionCode) & InstructionalMethod %in% c("Lecture", "Seminar", "Practicum"),
                          setdiff(LETTERS, SectionCode)[rank(SectionCode, na.last = FALSE)],
                          SectionCode
                        )
                    ) %>%
                    ungroup() %>%
                    arrange(Term, SubjectCode, CourseNum, SectionCode) %>%
                    left_join(gs_read(rv$SS, "Faculty")),
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
      # merge(the_duties(), all.x = TRUE, all.y = FALSE, by = "InstructionalMethod") %>%
      left_join(the_duties()) %>%
      mutate(Teaching = ifelse(is.na(Teaching), "Other", Teaching)) %>%
      group_by(Last, First, Teaching) %>% distinct()
    # print(head(sked))
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
    print(names(sked))
    
    sked 
    # %>%
    #     select(
    #       First, Last,
    #       Term, # SectionName,
    #       SubjectCode, CourseNum, SectionCode,
    #       FacultyLoad, # LocalMax, GlobalMax, 
    #       BuildingAndRoom, MeetingDays, 
    #       # MeetingTime,
    #       # Building, RoomNumber, Room,
    #       MeetingStart, MeetingEnd)
    #       # Faculty, 
    #       # InstructionalMethod, Notes)
  })
  
  output$loads <-
    renderDataTable( 
      fac_loads() 
    )
  
  output$schedule <- 
    renderDataTable( DT::datatable(the_sched(), filter = list(position = "top")) )
      # %>%  select_(.dots = intersect(names(the_data()), viewCols)))
      # %>%  select(intersect(names(the_data()), viewCols))
  
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
            choices = 
              if (!is.null(query()[["admin"]]) && query()[["admin"]] == "rpruim") {
                rev(
                  grep("\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE)
                ) 
              }  else {
                rev(
                  grep("^\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE)
                )
              }
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
              if (!is.null(query()[["admin"]]) && query()[["admin"]] == "rpruim") {
                rev( 
                  setdiff(
                    grep("\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE),
                    year1()
                  )
                )
              } else {
                rev(
                  setdiff(
                    grep("^\\d\\d\\d\\d-\\d\\d", gs_ws_ls(rv$SS), value = TRUE),
                    year1()
                  )
                )
              }
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
       geom_text(aes(x = Day, y = startTime + lubridate::minutes(20), label = CourseNum), 
                 angle = 0, size = 1.5, color = "black") +
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
       geom_text(aes(x = Day, y = startTime + lubridate::minutes(20), label = CourseNum), 
                 angle = 0, size = 1.5, color = "black") +
       facet_grid(Term ~ Last) +
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
           select(Term, SubjectCode, CourseNum, SectionCode, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, RoomMax, GlobalMax, Last, First, FacultyLoad) %>%
           unique()
       } else {
         the_sched() %>% 
           select(Term, SubjectCode, CourseNum, SectionCode, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, RoomMax, GlobalMax, Last, First, FacultyLoad) %>% 
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
           filter(Last == input$fac_click$panelvar1 & !is.na(SubjectCode)) %>% 
           select(Last, First, Term, SubjectCode, CourseNum, SectionCode, MeetingDays, MeetingStart, MeetingEnd, 
                  Room, RoomMax, GlobalMax, FacultyLoad) 
       }
     })
   
   output$fac_details2 <-
     renderDataTable({
       if (is.null(input$loads_rows_selected)) {
         the_sched() %>% 
           select(Last, First, Term, SubjectCode, CourseNum, SectionCode, MeetingStart, MeetingEnd, FacultyLoad, InstructionalMethod) %>%
           filter(FALSE)
       } else {
         rows <- as.integer(input$loads_rows_selected)
         print(rows)
         the_sched() %>%
           filter(Last %in% fac_loads()[["Last"]][rows]) %>% 
           # filter(Faculty %in% fac_loads()$Faculty[input$loads_rows_seleted] & !is.na(SubjectCode)) %>% 
           mutate(MeetingTime = paste(MeetingStart, "-", MeetingEnd)) %>%
           select(Last, First, Term, 
                  SubjectCode, CourseNum, SectionCode, MeetingDays, MeetingTime,
                  FacultyLoad, InstructionalMethod) %>%
           arrange(Last, First, Term, SubjectCode, CourseNum, SectionCode)
       }
     })
     
   
   output$columnsUI <- 
     renderUI({
       checkboxGroupInput(
         "comp_columns", "include", 
         choices = 
           c("Term", "SubjectCode", "CourseNum", "SectionCode", "MeetingDays", 
                     "MeetingStart", "MeetingEnd", "Room", "RoomMax", "GlobalMax", "Faculty", 
                     "FacultyLoad", "InstructionalMethod"),
          selected = 
           c("Term", "SubjectCode", "CourseNum", 
             # "MeetingDays",  "MeetingStart", "MeetingEnd", "Room", "RoomMax", "GlobalMax", "Faculty", 
               "FacultyLoad", "InstructionalMethod")
       )                        
     })
   
   output$comparison_data <- 
     renderDataTable({
       DT::datatable(
         filter = list(position = "top"),
         bind_rows(
           the_sched() %>% 
             select_(.dots = c(intersect(names(the_sched()), input$comp_columns), "FacultyLoad")) %>% 
             group_by_(.dots = intersect(names(the_sched()), input$comp_columns)) %>%
             summarise(n = n(), load = sum(FacultyLoad, na.rm = TRUE)) %>%
             ungroup() %>%
             anti_join(
               the_sched2() %>% 
                 select_(.dots = c(intersect(names(the_sched2()), input$comp_columns), "FacultyLoad")) %>% 
                 group_by_(.dots = intersect(names(the_sched2()), input$comp_columns)) %>%
                 mutate(n = n(), load = sum(FacultyLoad, na.rm = TRUE)) 
             ) %>%
             mutate(Schedule = "Primary"),
           the_sched2() %>% 
             select_(.dots = c(intersect(names(the_sched2()), input$comp_columns), "FacultyLoad")) %>% 
             group_by_(.dots = intersect(names(the_sched2()), input$comp_columns)) %>%
             summarise(n = n(), load = sum(FacultyLoad, na.rm = TRUE)) %>%
             ungroup() %>%
             anti_join(
               the_sched() %>% 
                 select_(.dots = c(intersect(names(the_sched()), input$comp_columns), "FacultyLoad")) %>% 
                 group_by_(.dots = intersect(names(the_sched()), input$comp_columns)) %>%
                 mutate(n = n(), load = sum(FacultyLoad, na.rm = TRUE))
             ) %>%
             mutate(Schedule = "Secondary")
             ) %>% # end bind_rows()
           select_(.dots = 
              c("Schedule", intersect(names(the_sched()), input$comp_columns), "n", "load")),
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