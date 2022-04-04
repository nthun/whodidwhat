# Function to read multiple header chore sheets

library(tidyverse)
library(readxl)

get_range <- function(path, sheet = 1, range = "A1:R1") {
    readxl::read_xlsx(
        path = path,
        sheet = sheet,
        range = range,
        col_names = FALSE
    ) %>%
        
        unlist(use.names = FALSE) %>%
        zoo::na.locf()
    
}

get_vars <- function(path, sheet = 1, range = "A1:R1") {
    first_row <- get_range(path)
    
    readxl::read_xlsx(
        path = path,
        sheet = sheet,
        range = "A2:R2",
        col_names = FALSE
    ) %>%
        purrr::flatten() %>%
        unlist(use.names = FALSE) %>%
        paste(first_row, ., sep = "_") %>%
        stringr::str_remove("_NA|-_-")
}

read_chore_sheet <- function(path, sheet = 1, range = "A1:R1") {
    suppressMessages(
        readxl::read_xlsx(
            path = path,
            sheet = sheet,
            skip = 2,
            trim_ws = TRUE,
            col_names = get_vars(path, sheet, range)
        ) %>%
            tidyr::fill(Room) %>%
            dplyr::mutate(Room = forcats::fct_inorder(Room),
                          Task = forcats::fct_inorder(Task))
    )
}

sheet_to_long <- function(df){
    df %>% 
        pivot_longer(Rozi_H:Tomi_V,
                     names_to = c("person", "day"),
                     names_pattern = "(.*)_(.*)",
                     values_to = "done", 
                     values_drop_na = TRUE) %>% 
        filter(done)
}

sum_tasks <- function(df){
    
    df %>% 
        group_by(person, Room, Task) %>% 
        summarise(done = sum(done), .groups = "drop") %>% 
        pivot_wider(names_from = person,
                    values_from = done) %>% 
        arrange(Room, Task)
    
}
