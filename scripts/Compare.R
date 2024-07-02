library(text.alignment)
library(tidyverse)

a <- read_lines("texts/fileb_1929.txt") 
empty <- which(a == "")
a <- a[-empty] |> 
  str_c(collapse = "\n") |> 
  str_replace_all("—", "-")



b <- read_lines("texts/fileb_1994.txt")  |> 
  str_c(collapse = "\n") |> 
  str_replace_all("—", "-") |> 
  str_replace_all("–", "-")

res <- smith_waterman(a, b, type = "words",
                      tokenizer = function(x) unlist(strsplit(x, "\\s"))) 

tbl <- tibble(translation_1929 = res$a$alignment$tokens,
              translation_1994 = res$b$alignment$tokens)
 

tbl_new <- tbl |> 
  mutate(all_edits = case_when(str_detect(translation_1929, "#+") ~ paste0("<ins>", translation_1994, "</ins>"),
                       .default = translation_1994)) |>
  mutate(all_edits = case_when(str_detect(translation_1994, "#+") ~ paste0("<del>", translation_1929, "</del>"),
                                        .default = all_edits)        
           
         ) 


tbl_new2 <- tbl_new |> 
  mutate(all_edits = case_when(all_edits == "Сократ." ~ "\n\n<strong>Сократ.</strong>",
                               all_edits == "Протарх." ~ "\n\n<strong>Протарх.</strong>",
                               all_edits == "Филеб." ~ "\n\n<strong>Филеб.</strong>",
                               .default = all_edits))
  

text_ed <- tbl_new2 |> 
  pull(all_edits) |> 
  str_c(collapse = " ")


# write file
my_qmd <- str_glue("---
title: 'Переводы Филеба: 1929 vs. 1994'
format: html
theme: 
 - cosmo
 - styles.scss
fontsize: '14pt'
---", "\n", text_ed, "\n")

write_lines(my_qmd, "index.qmd")
