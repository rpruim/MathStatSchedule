
library(lubridate)
n <- 5
filler <- matrix("-", nrow = n, ncol = n,
                 dimnames = list(NULL, paste0("V", seq_len(n))))

## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_token.rds")
# ss <- gs_new("10_read-write-private-sheet",
#              row_extent = n, col_extent = n, input = filler)

## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

googlesheets::gs_auth(token = "shiny_token.rds")
ss <- googlesheets::gs_title("MathStatSchedules")

char2Time <- function(x) { 
  ymd("2015-02-02") + lubridate::hm(x)
}

courseLevel <- function(x) {
  ifelse(is.na(x), NA, as.numeric(paste0(substr(x, 1, 1), "00")))
}
  

viewCols <- c("Term", "SubjectCode", "CourseNum", 
              "BuildingAndRoom", "MeetingDays", "MeetingTime",
              "FacultyLoad", "Faculty", "LocalMax", "GlobalMax", "RoomCapacity" 
              )

  
              