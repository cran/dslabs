library(readr)
library(tidyverse)
library(stringr)
library(rvest)

##polls
url <- "http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv"
polls_us_election_2016 <- read_csv(url)
polls_us_election_2016 <- filter(polls_us_election_2016, type == "polls-plus")
polls_us_election_2016$startdate <- as.Date(polls_us_election_2016$startdate,"%m/%d/%Y") ##turn enddate into date
polls_us_election_2016$enddate <- as.Date(polls_us_election_2016$enddate,"%m/%d/%Y") 
polls_us_election_2016 <- select(polls_us_election_2016, state, startdate, enddate, pollster, grade, samplesize, population, rawpoll_clinton, rawpoll_trump, rawpoll_johnson, rawpoll_mcmullin, adjpoll_clinton, adjpoll_trump, adjpoll_johnson, adjpoll_mcmullin)
polls_us_election_2016 <- as.data.frame(polls_us_election_2016)
polls_us_election_2016 <- mutate(polls_us_election_2016, 
                                 state = factor(state), 
                                 pollster = factor(pollster), 
                                 grade = factor(grade, levels = c("D","C-","C","C+","B-","B","B+","A-","A","A+")))

##2016 results
url <- "https://en.wikipedia.org/w/index.php?title=2016_United_States_presidential_election&oldid=1265623400"
tmp <- read_html(url) |> html_nodes("table") |> _[[43]] |> html_table()  |>
  _[-c(1,58,59), c(1, 23, seq(2, 17, 3), seq(4, 19, 3))]
x <- tmp |> setNames(c("state",  "total", tolower(str_remove(names(tmp[3:8]), "/.*")),
                       paste0("EV",1:6))) |>
  mutate(across(-state, ~ if_else(. == "–", "0", .)))
x$electoral_votes <- select(x, contains("EV")) |> apply(1, function(x) sum(as.numeric(x), na.rm = TRUE))
x <- select(x, -contains("EV"))
x <- x |> mutate(across(-c(state, electoral_votes), parse_number)) |>
  mutate(state = str_trim(str_replace(x$state,"Tooltip.*$|†|\\[\\d+\\]", ""))) 
x$sum <- select(x, -c(state, total, electoral_votes)) |> rowSums()
x <- x |> mutate(total = if_else(sum > total, sum, total)) |>
  select(-sum) |>  
  mutate(across(-c(state, total, electoral_votes), ~ . / total * 100)) |>
  select(-total) |>
  arrange(desc(electoral_votes)) |>
  relocate(electoral_votes, .after = state) |>
  mutate(state = str_replace_all(state, c("ME-" = "Maine CD-", "NE-" = "Nebraska CD-")))

## we reorder for illustrate how join works
results_us_election_2016 <- as.data.frame(x) %>% arrange(desc(electoral_votes))  
save(polls_us_election_2016, results_us_election_2016, file = "data/polls_us_election_2016.rda", compress = "xz")
