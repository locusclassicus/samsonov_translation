library(pdftools)
library(tesseract)
library(readr)
doc <- pdf_ocr_text("./texts/fileb_1929.pdf",
                    language = "rus")
write_lines(doc, file = "./texts/fileb_1929.txt")
