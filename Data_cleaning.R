## iIMPORTAR SOLO UN JSON

library("rjson") 

# Give the input file name to the function. COMO UNA LISTA
result <- rjson::fromJSON(file = "C:/Documentos/json/tweet_by_keyword_alergia_upTo_01_11_2020__07_22_33.txt") 
glimpse(result)

## poniendolo bonito

rgx_split <- "\\."
n_cols_max <-
  data_raw %>%
  pull(name) %>% 
  stringr::str_split(rgx_split) %>% 
  map_dbl(~length(.)) %>% 
  max()
n_cols_max
##5

library(tibble)
data_raw <- enframe(unlist(result))
data_raw
View(data_raw)

data_raw %>% tidyr::separate(name, into = c(paste0("x", 1:5)), fill = "right")
 #OR

nms_sep <- paste0("name", 1:n_cols_max)
data_sep <-
  data_raw %>% 
  tidyr::separate(name, into = nms_sep, sep = rgx_split, fill = "right")
View(data_sep)

data_filt <-
  data_sep %>%
  filter(
    (
      name1 == "created_at"
    ) |
      (
        name1 == "retweeted"
      ) | (
        name1 == "id"  ) |
      (
        name1 == "text"
      ) | (
        name1 == "lang"  ) |
      (
        name1 == "user" &
          name2 == "id") |
    (
        name1 == "user" &
          name2 == "location" ))
    
data_filt


data_clean1 <-
  data_filt %>%
  select(name1, name2, name3, value) %>%
  mutate(user = if_else(name2 == "id", value, NA_character_)) %>%
  mutate(text = if_else(name1 == "text", value, NA_character_)) %>%
    mutate(datetime = if_else(
    name1 == "created_at",
    stringr::str_replace_all(value, "\\s?T\\s?", " ") %>% stringr::str_replace("Z$", ""),
    NA_character_
  ))
data_clean1


## IMPORTAR COMO DATA FRAME

json_file <- stream_in(file("./tweet_by_keyword_alergia_upTo_01_11_2020__07_22_33.json"))
dplyr::glimpse(json_file)
##or
dat <- fromJSON(sprintf("[%s]", paste(readLines("./tweet_by_keyword_alergia_upTo_01_11_2020__07_22_33.json"), collapse=",")))
## OR pero no se...
system.time(bitly02 <- ndjson::stream_in(myJSON))
dplyr::glimpse(bitly02)
# Rows: 55,840
# Columns: 1,517

### cleaning

library(tidyr)
library(dplyr)

clean.text = function(x)
{
  x = gsub("rt", "", x) # remove Retweet
  x = gsub("@\\w+", "", x) # remove at(@)
  x = gsub("[[:punct:]]", "", x) # remove punctuation
  x = gsub("[[:digit:]]", "", x) # remove numbers/Digits
  x = gsub("http\\w+", "", x)  # remove links http
  x = gsub("[ |\t]{2,}", "", x) # remove tabs
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  try.error = function(z) #To convert the text in lowercase
  {
    y = NA
    try_error = tryCatch(tolower(z), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(z)
    return(y)
  }
  x = sapply(x, try.error)
  return(x)
}

limpitos <- clean.text(myJSON)

class(myJSON)
