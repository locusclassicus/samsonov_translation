library(text.alignment)
library(tidyverse)

# read files 
a <- tibble(a = read_lines("texts/fileb_1929.txt")) |> 
  filter(a != "") |>
  mutate(a = str_replace_all(a, "—", "-")) |> 
  mutate(a = str_replace_all(a, "–", "-")) |> 
  pull(a) |> 
  str_c(collapse = "\n")


b <- tibble(b = read_lines("texts/fileb_1994.txt"))  |> 
  filter(b != "") |> 
  mutate(b = str_replace_all(b, "—", "-")) |> 
  mutate(b = str_replace_all(b, "–", "-")) |> 
  pull(b) |> 
  str_c(collapse = "\n")

#tokenizer

tokenizer <- function(x){
  x |> 
    str_replace_all("([:punct:])", " \\1") |> 
    str_split(" ") |> 
    unlist()
}

# compare
res <- smith_waterman(a, b, type = "words",
                      tokenizer = tokenizer) 

tbl <- tibble(translation_1929 = res$a$alignment$tokens,
              translation_1994 = res$b$alignment$tokens)
 

# add html tags
tbl_new <- tbl |> 
  mutate(all_edits = case_when(str_detect(translation_1929, "#+") ~ paste0("<ins>", translation_1994, "</ins>"),
                       .default = translation_1994)) |>
  mutate(all_edits = case_when(str_detect(translation_1994, "#+") ~ paste0("<del>", translation_1929, "</del>"),
                                        .default = all_edits)
         ) 



text_ed <- tbl_new |> 
  pull(all_edits) |> 
  str_c(collapse = " ") |> 
  str_squish() |>
  str_replace_all("(\\s+)([[:punct:]])", "\\2")

text_ed2 <- text_ed |> 
  str_replace_all("([[:punct:]]\\s)(Сократ|Протарх|Филеб)(\\.)",
                  "\\1 \\n<strong>\\2</strong>\\3")



# write file
my_qmd <- str_glue("---
title: 'Переводы Филеба: 1929 vs. 1994'
format: html
theme: 
 - cosmo
 - styles.scss
fontsize: '14pt'
---", "\n", text_ed2, "\n")

write_lines(my_qmd, "index.qmd")
