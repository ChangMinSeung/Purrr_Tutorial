library(purrr)

data("mtcars")
attach(mtcars)

fun_list <- list(mean, median, sd)
map(fun_list, ~ mtcars %>% map_dbl(.x))

library(tidyverse)
library(purrr)
library(gapminder)
library(broom)
library(modelr)
library(gridExtra)
library(stringr)
library(ggthemes)
library(extrafont)
loadfonts()

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

#nest()함수를 활용하여 중첩 데이터프레임(nested dataframe)을 도입하여 데이터를 group_by로 #정리하고 나서 회귀모형 함수를 group_by 즉, 국가별로 함수형 프로그래밍을 적용하여 수행한다.

#
by_county <- gapminder %>%
  group_by(continent, country) %>%
  nest()
by_county$data[[1]]

#
country_model <- function(df) {
  lm(lifeExp ~ year, data=df)
}

#
models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

names(by_country$model) <- by_country$country

by_country$model["Korea, Rep."]

#
by_country <- by_country %>% 
  mutate(
    tidy    = map(model, broom::tidy),
    glance  = map(model, broom::glance),
    rsq     = glance %>% map_dbl("r.squared"),
    augment = map(model, broom::augment)
  )

by_country$tidy["Korea, Rep."]

#
by_country$glance["Korea, Rep."]

#
unnest(by_country, data)    

#
unnest(by_country, tidy)

#
unnest(by_country, glance, .drop = TRUE) 


#starwars#
##########
## http://swapi.co/
# install_github("ironholds/rwars", ref = "0.5.0")
library(rwars)
library(purrr)

# recursive function to travel down next pages
get_all <- function(x = NULL, old_data = NULL, fun, ...){
  data <- fun(x, ...)
  next_url <- getElement(data, "next")
  if(is_null(next_url)) {
    return(append(old_data, list(data)))
  } else {
    get_all(next_url, append(old_data, list(data)), fun, ...)
  }
}

entities <- c("species", "people", "films", "vehicles", "starships",
              "planets")

all_ents <- map(entities, 
                ~ get_all(fun = match.fun(paste0("get_all_", .x)), parse_result = TRUE))

all_ents <- map(all_ents, ~ transpose(.x)[["results"]] %>% flatten())

map2(entities, all_ents, ~ assign(.x, .y, envir = globalenv()))

length(people)
str(people)
str(people[[1]])
str(people[1])

luke <- people[[1]]
luke$starships
length(luke$starships)

library(tidyverse)
map(people, ~ length(.x$starships))

#
load("planet_lookup.rda")
str(planet_lookup)

people <- people %>% set_names(map_chr(people, "name"))
map_int(people, ~ length(.x[["starships"]]))
map_chr(people, ~ .x[["hair_color"]])
map_lgl(people, ~ .x[["gender"]] == "male")

map_chr(people,
        ~ readr::parse_number(.x[["mass"]],
                              na = "unkown"))

# which film has the most characters?
map(films, "characters") %>%
  map_int(length) %>%
    set_names(map_chr(films, "title")) %>%
      sort()
