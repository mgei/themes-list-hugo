library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(janitor)
library(lubridate)

process_data <- function(data) {
  # Split each string by newline
  split_data <- strsplit(data, "\n")
  
  # Create a named list
  clean_data <- lapply(split_data, function(item) {
    key <- item[1]
    value <- paste(item[-1], collapse = ", ")
    return(setNames(list(value), key))
  })
  
  # Combine all lists into one
  combined_data <- do.call(c, clean_data)
  return(combined_data)
}

html <- read_html("https://themes.gohugo.io/")

links <- html %>% 
  html_nodes("a") %>% 
  html_attr("href")

links_themes <- links |> 
  as_tibble() |> 
  filter(str_detect(value, "themes/")) |> 
  pull(value)

themes <- tibble()
for (i in 1:length(links_themes)) {
  cat(i, "\n")
  theme_page <- read_html(links_themes[i])
  
  theme_data <- theme_page |> 
    html_nodes(".mb2") |> 
    html_text()
  
  if (length(theme_data) > 0) {
    theme_data_list <- process_data(theme_data)  
  } else {
    theme_data_list <- c(`Author:` = NA_character_)
  }
  
  
  theme_descr <- theme_page |> 
    html_nodes(".description") |> 
    html_text()
  if (length(theme_descr) != 1) {
    theme_descr <- NA_character_
  }
  
  theme_name <- theme_page |> 
    html_nodes(".w-50-l .primary-color") |> 
    html_text()
  if (length(theme_name) != 1) {
    theme_name <- NA_character_
  }
  
  theme_download <- theme_page %>% 
    html_nodes("a.bg-accent-color.br2") %>%
    keep(~ grepl("Download", html_text(.))) %>%
    html_attr("href")
  if (length(theme_download) != 1) {
    theme_download <- NA_character_
  }
  
  theme_demo <- theme_page %>% 
    html_nodes("a.bg-accent-color.br2") %>%
    keep(~ grepl("Demo", html_text(.))) %>%
    html_attr("href")
  if (length(theme_demo) != 1) {
    theme_demo <- NA_character_
  }
  
  theme_text <- theme_page |> 
    html_node(".overflow-hidden") |> 
    html_text()
  if (length(theme_text) != 1) {
    theme_text <- NA_character_
  }
  
  temp <- as_tibble(theme_data_list) |> 
    mutate(name = theme_name,
           description = theme_descr,
           download = theme_download,
           demo = theme_demo,
           text = theme_text) |> 
    clean_names()
  
  themes <- bind_rows(themes, temp)
}

themes <- themes |> 
  mutate(updated = ymd(updated))

str_abbreviate <- function(string, len = 30L) {
  if (is.na(string)) {
    return(string)
  }
  
  if (str_length(string) > (len - 3)) {
      return(paste0(str_sub(string, end = len - 3), "..."))
  } else {
    return(string)
  }
}

str_abbreviate_V <- Vectorize(str_abbreviate)


themes |> 
  transmute(name, 
            description = str_abbreviate_V(description),
            updated, author, license, git_hub_stars, download)


