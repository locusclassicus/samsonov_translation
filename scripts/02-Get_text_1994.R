library(tidyverse)
library(rvest)
library(xml2)


my_url <- "https://plato.spbu.ru/TEXTS/PLATO/LosevH/0301.htm"

# grab texts
get_text <- function(url) {
  p <- read_html(url) |> 
    html_elements("p") 
  i <- p |> 
    html_elements("i")
  xml_remove(i)
  p |> 
    html_text2()
}

text <- get_text(my_url)

# clean text
clean_text <- function(text){
  text |> 
    str_remove_all("[\u0041-\u007F]") |> 
    str_remove_all("[0-9]") |> 
    str_remove_all("[[\u0370-\u03FF][\U1F00-\U1FFF]]") |> 
    str_squish()
} 

text_clean <- clean_text(text)
#write_lines(text_clean, file = "texts/fileb_1994.txt")
# some manual cleaning done 
read_lines("texts/fileb_1994.txt")
