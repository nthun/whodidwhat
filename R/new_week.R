# Create blank sheet
# BUG: Have to read chores!
# TODO: add option to read new chore sheet, or download automatically if not present
# TODO: Get sheet end automatically!

source("R/read_chore_sheets.R")

chores  <- read_chore_sheet()

new_week <- function(chores_gid,
                     when = lubridate::today(),
                     sheet_end = "R"){
    
    # Get next week name
    new_week <- as.character(lubridate::floor_date(x = as.Date(when), 
                                                   unit = "weeks", 
                                                   week_start = 1))

    # Duplicate template
    googlesheets4::sheet_copy(from_ss = chores_gid, 
                              to_sheet = new_week, 
                              .before = 1)
    
    # Remove all checks from a week
    # TODO: Flood cells in a single call
    # Reset Rozi
    googlesheets4::range_flood(ss = chores_gid, 
                               range = stringr::str_glue("{new_week}!D3:J{nrow(chores)+2}"),
                               cell = FALSE)
    
    # Reset Tomi
    googlesheets4::range_flood(ss = chores_gid, 
                               range = stringr::str_glue("{new_week}!L3:R{nrow(chores)+2}"),
                               cell = FALSE)
    
    
}
