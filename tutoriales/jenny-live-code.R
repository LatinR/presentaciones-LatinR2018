#' ---
#' title: "Live code from purrr workshop at Latin-R"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' Use `rmarkdown::render()` on this or, in RStudio, click on the "Compile Report" spiral-notebook icon.
#'
#' ## Where to find this document
#'
#' Shortlink humans can type:
#'
#'   * <http://bit.ly/jenny-live-code>
#'
#' Horrible link that reveals how this is done:
#'
#'   * <https://www.dropbox.com/s/2b8mi4rir23pvnx/jenny-live-code.R?raw=1>
#'
#' Using the `raw=1` query trick for rendering a DropBox-hosted file in the
#' browser:
#'
#'   * <https://www.dropbox.com/en/help/desktop-web/force-download>
#' learned from [Michael Levy](https://twitter.com/ucdlevy).
#'
#' How this works:
#'
#'   * I code live in an R script locally. I save often.
#'   * This file lives in a directory synced to DropBox.
#'   * You open the DropBox file at <http://bit.ly/jenny-live-code> and refresh
#'   as needed.
#'   * Should allow you to see, copy, paste everything I've typed and save the
#'   entire transcript at the end. This file is highly perishable, so save your
#'   own copy if you want it.
#'   * Every now and then the refresh won't work. Just re-open from from the
#'   bit.ly link: <http://bit.ly/jenny-live-code>
#'
#+ setup, include = FALSE
knitr::opts_chunk$set(error = TRUE, collapse = TRUE)

#+ live-code

#' ## Workshop material starts here

library(purrr)
library(repurrrsive)
help(package  = "repurrrsive")

## How many elements are in got_chars?
length(got_chars)

## Who is the 9th person listed in got_chars?
## What information is given for this person?
got_chars[[9]]
str(got_chars[[9]])
str(got_chars[[9]], list.len = 4)
#View(got_chars[[9]])
#View(got_chars)

## What is the difference between got_chars[9] and got_chars[[9]]?
got_chars[1]
got_chars[[1]]
str(got_chars[1], max.level = 1)
str(got_chars[[1]], max.level = 0)

## What is the length of each GoT character's aliases?
daenerys <- got_chars[[9]]
length(daenerys[["aliases"]])

map(got_chars, ~ length(.x[["aliases"]]))

## How many x does each (GoT or SW) character have?
## (x = 3 titles, allegiances, vehicles, starships)
map(got_chars, ~ length(.x[["titles"]]))
map(sw_people, ~ length(.x[["starships"]]))

# What's each character's name?
map_chr(got_chars, ~.x[["name"]])
map_chr(sw_people, ~.x[["name"]])

# What color is each SW character's hair?
map_chr(sw_people, ~ .x[["hair_color"]])

# Is the GoT character alive?
map_lgl(got_chars, ~ .x[["alive"]])

# Is the SW character female?
map_lgl(sw_people, ~ .x[["gender"]] == "female")

# How heavy is each SW character?
map_dbl(sw_people, ~ .x[["mass"]])
map_chr(sw_people, ~ .x[["mass"]])
map_dbl(sw_people, ~ as.character(.x[["mass"]]))
map_dbl(sw_people, ~ readr::parse_number(.x[["mass"]]))

## Explore a GoT or SW list and find a new element to look at
## Extract it across the whole list with name and position shortcuts for .f
## Use map_TYPE() to get an atomic vector as output
got_chars %>%
  map_chr("culture")

sw_people %>%
  map_chr("birth_year")

got_chars %>%
  map("allegiances") %>%
  View()

## What if the thing you are extracting is not there or
## length 0 or of lenght > 1?

map(sw_vehicles, "pilots", .default = NA)

map_chr(sw_vehicles, list("pilots", 1), .default = NA)

## Names make life nicer!
got_chars_named <- set_names(got_chars, map_chr(got_chars, "name"))
View(got_chars_named)

got_chars_named %>%
  map_lgl("alive")

## Challenge:
## Create a named copy of a GoT or SW list with set_names().
## Find an element with tricky presence/absence or length.
##
## Extract it many ways:
## - by name
## - by position
## - by list("name", pos) or c(pos, pos)
## - use .default for missing data
## - use map_TYPE() to coerce output to atomic vector

## is 'books' tricky? yeah, some characters are in 0 books
## (how is that even possible?!) and others are in 5
got_chars_named %>%
  map("books") %>%
  map_int(length) %>%
  table()

which(names(got_chars[[1]]) == "books")

identical(
  map(got_chars_named, "books"),
  map(got_chars_named, 15)
)

map(got_chars_named, "books", .default = "no book!")

got_chars_named %>%
  map("books", .default = "no book!") %>%
  map_chr(paste, collapse = ", ") %>%
  map_chr(substr, start = 1, stop = 60)

## Challenge (pick one or more):

##  Which SW film has the most characters?
#View(sw_films)
film_names <- map_chr(sw_films, "title")
sw_films %>%
  set_names(film_names) %>%
  map("characters") %>%
  map_int(length) %>%
  sort() %>%
  tail(1)
## I think it's Attack of the Clones?

##  Which SW species has the most possible eye colors?
library(tidyverse)

df <- tibble(
  who = map_chr(sw_people, "name"),
  eye_color = map_chr(sw_people, "eye_color"),
  species = map_chr(sw_people, "species", .default = NA)
)
df %>%
  group_by(species) %>%
  summarize(eye_color_n = n_distinct(eye_color)) %>%
  arrange()
## http://swapi.co/api/species/1/

View(sw_species)
species_urls <- sw_species %>%
  map_chr("url")
this_one <- which(species_urls == "http://swapi.co/api/species/1/")
sw_species[[this_one]][["name"]]
## Human!

## easier way :(
sw_species_names <- map_chr(sw_species, "name")
sw_species %>%
  set_names(sw_species_names) %>%
  map_chr("eye_colors") %>%
  map(~ strsplit(.x, split = ",")[[1]]) %>%
  map_int(length) %>%
  sort()

people_names <- map(sw_people, "title")
sw_films %>%
  set_names(film_names) %>%
  map("characters")

##  Which GoT character has the most allegiances? Aliases? Titles?
##
##  Which GoT character has been played by multiple actors?

## walk
library(tidyverse)
library(gapminder)

countries <- c("Argentina", "Brazil", "Canada")
gap_small <- gapminder %>%
  filter(country %in% countries, year > 1996)
gap_small

filename <- paste0("Argentina", ".csv")
dataset <- filter(gap_small, country == "Argentina")
write_csv(dataset, filename)

write_one <- function(x) {
  filename <- paste0(x, ".csv")
  dataset <- filter(gap_small, country == x)
  write_csv(dataset, filename)
}

map(countries, write_one)
walk(countries, write_one)
list.files(pattern = "*.csv")

## doing the inverse
library(tidyverse)

csv_files <- list.files(pattern = "*.csv")
csv_files

map_dfr(csv_files, ~ read_csv(.x))
