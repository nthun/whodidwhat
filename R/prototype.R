library(tidyverse)
library(googlesheets4)
library(lubridate)
library(openxlsx)

chores_gid <- "1cqq8YWKWbk11DuFPvTJa9hYSXUg_WL6Znh4KnrXPz6M"

# Read tasks and rooms
rooms_task <- 
    range_read(chores_gid, sheet = 1, n_max = 1) %>% 
    select(-(1:2)) %>% 
    pivot_longer(everything(), 
                 names_to = "room",
                 values_to = "task") %>% 
    mutate(room = if_else(str_starts(room, "\\.\\.\\."), 
                          NA_character_, 
                          room)) %>% 
    fill(room)


chores_long <- 
    chores %>% 
    pivot_longer(-c(Ki, Nap),
                 names_to = "task",
                 values_to = "done") %>% 
    left_join(rooms_task, by = "task") %>% 
    filter(done)


# Create a new week template ---------------------------------------------------
# Get next week name
new_week <- as.character(floor_date(today(), unit = "weeks", week_start = 1))
# Get the last column of the sheet
sheet_end <- int2col(nrow(rooms_task)+2)


# Duplicate template
sheet_copy(from_ss = chores_gid, 
           to_sheet = new_week, 
           .before = 1)

# Remove all checks from a week
# TODO: Flood cells in a single call
# Reset Rozi
range_flood(ss = chores_gid, 
            range = str_glue("{new_week}!C4:{sheet_end}10"),
            cell = FALSE)

# Reset Tomi
range_flood(ss = chores_gid, 
            range = str_glue("{new_week}!C12:{sheet_end}18"),
            cell = FALSE)


# V2.0 recode ------------------------------------------------------------------
# Read tasks and rooms
first_row <- range_read(chores_gid, 
                        sheet = 1, 
                        range = "A1:R1", 
                        col_names = FALSE) %>% 
    unlist(use.names = FALSE) %>%
    zoo::na.locf()

chore_vars <-
    range_read(chores_gid, 
               sheet = 1, 
               range = "A2:R2", 
               col_names = FALSE) %>% 
    flatten() %>% 
    unlist(use.names = FALSE) %>%
    paste(first_row, ., sep = "_") %>% 
    str_remove("_NA|-_-")

# rooms_task <-
#     range_read(chores_gid, sheet = 1, skip = 1) %>% 
#     select(1:2) %>% 
#     fill(Room)

chores <-
    range_read(chores_gid,
               skip = 2,
               trim_ws = TRUE,
               col_names = chore_vars) %>%
    fill(Room)

chores_long <- 
    chores %>% 
    pivot_longer(Rozi_H:Tomi_V,
                 names_to = c("person", "day"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "done", 
                 values_drop_na = TRUE) %>% 
    filter(done)

# Create a new week template ---------------------------------------------------
# Get next week name
new_week <- as.character(floor_date(today(), unit = "weeks", week_start = 1))
# Get the last column of the sheet
sheet_end <- "R"


# Duplicate template
sheet_copy(from_ss = chores_gid, 
           to_sheet = new_week, 
           .before = 1)

# Remove all checks from a week
# TODO: Flood cells in a single call
# Reset Rozi
range_flood(ss = chores_gid, 
            range = str_glue("{new_week}!D3:J{nrow(chores)+2}"),
            cell = FALSE)

# Reset Tomi
range_flood(ss = chores_gid, 
            range = str_glue("{new_week}!L3:R{nrow(chores)+2}"),
            cell = FALSE)
