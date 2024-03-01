library(pdftools)
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package = "dslabs")
x <- str_split(pdf_text(fn), "\n")
pr_death_counts <- map_df(x, function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n <= 3), 
           which(n >= 28 & n <= 31), tail_index:length(s))
  
  if (month == "FEB") {
    feb29 <- s[str_detect(s, "^29\\s+")] |> str_remove("29\\s+") |> parse_number()
  }
  s <- s[-out] |>  
    str_remove_all("[^\\d\\s]") |> ## remove things that are not digits or space
    str_trim() |> 
    str_split_fixed("\\s+", n = 6)  ## split by any space
  
  s <- s[,1:5]
  colnames(s) <- c("day", header)
  s <- s |> as_tibble(validate = FALSE) |> 
    mutate(month = month, day = as.numeric(day)) |>
    pivot_longer(-c(day, month), names_to = "year", values_to = "deaths") |>
    mutate(deaths = as.integer(deaths), month = str_to_title(month)) |>
    mutate(month = if_else(month == "Ago", "Aug", month)) |>
    mutate(month = match(month, month.abb)) |>
    mutate(date = make_date(year, month, day)) |>
    select(date, deaths) 
  if (month == "FEB") {
    s <- bind_rows(s, data.frame(date = make_date(2016, 2, 29), deaths = feb29)) 
  }
  return(s)
}) |> arrange(date) |> filter(date <= make_date(2018, 4, 15)) |> as.data.frame()

save(pr_death_counts, file = "data/pr-death-counts.rda", compress = "xz", version = 2)
  