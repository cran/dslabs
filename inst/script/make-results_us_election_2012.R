##2012 results
library(rvest)
library(readr)
library(tidyverse)
library(stringr)

url <- "https://en.wikipedia.org/w/index.php?title=2012_United_States_presidential_election&oldid=1264588444"
tmp <- read_html(url) |> html_table() |> _[[25]] |> _[-c(1,58), c(1, 20, seq(2, 11, 3), c(4, 7))]
x <- tmp |> setNames(c("state",  "total", 
                       tolower(str_replace(names(tmp[3:6]), "^([^A-Z]*[A-Z][a-z]*[^A-Z]*){2}[A-Z][a-z]*", "\\1")), 
                       paste0("EV",1:2))) |>
  mutate(across(-state, ~ if_else(. == "–", "0", .)))
x$electoral_votes <- apply(x[,c("EV1","EV2")], 1, function(x) sum(as.numeric(x), na.rm = TRUE))
x <- select(x,-c("EV1","EV2"))
x <- x |> mutate(across(-c(state, electoral_votes), parse_number)) |>
  mutate(state = str_replace(x$state,"Tooltip.*$|†|\\[\\d+\\]|District of Columbia", "")) 
x$sum <- rowSums(x[,3:5])
x <- x |> mutate(total = if_else(sum > total, sum, total)) |>
  select(-sum) |>  
  mutate(across(-c(state, total, electoral_votes), ~ . / total * 100)) |>
  select(-total) |>
  arrange(desc(electoral_votes)) |>
  relocate(electoral_votes, .after = state) |>
  mutate(state = str_replace_all(state, c("ME-" = "Maine CD-", "NE-" = "Nebraska CD-")))



results_us_election_2012 <- as.data.frame(x)
save(results_us_election_2012, file = "data/results_us_election_2012.rda", compress = "xz")

