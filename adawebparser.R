#run adacrawler.R before
library(stringr)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
total.list <- vector("list")
for(j in 1:length(ada.condition)) {
  url <- ada.condition[[j]]
  #url <- "https://ada.com/conditions/acne-vulgaris/"
  rawhtml <- read_html(url)
  title <- rawhtml %>% html_node("head") %>% html_nodes("title") %>%
    html_text(trim = TRUE) %>% regmatches(.,regexpr("[A-Za-z].*? «",.)) %>%
    substr(.,0,nchar(.) - 2)
  
  bodyhtml <- rawhtml %>% html_nodes("script")
  symptoms.newline.raw <- xml_contents(bodyhtml[6]) %>% html_text(trim=TRUE) %>%
    regmatches(.,gregexpr("@@symptom.*?@@",.)) %>%
    regmatches(.,gregexpr("-.*?\\\\n",.))
  
  symptoms.newline <- list()
  for (i in 1:length(symptoms.newline.raw[[1]])) {
    symptoms.newline[i] <- gsub("[^a-zA-Z[:blank:]]", "", symptoms.newline.raw[[1]][i]) %>%
      substr(.,0,nchar(.) - 1)
  }
  
  if(is.na(symptoms.newline)) {
    symptoms.newline <- xml_contents(bodyhtml[6]) %>% html_text(trim=TRUE) %>%
      regmatches(.,gregexpr("@@symptom.*?@@",.)) %>%
      regmatches(.,gregexpr("\\*{2}.*?\\*{2}",.))
  }
  # symptoms <- xml_contents(bodyhtml[6]) %>% html_text(trim=TRUE) %>% 
  #   regmatches(., gregexpr("@@sy.*?\\*{2}.*?\\*{2}",text = .)) %>% 
  #   regmatches(.,gregexpr("\\*{2}.*\\*{2}",text = .)) %>% substr(.,3,nchar(.) - 2)
 # print(title)
  #print(symptoms.newline)
  print(title)
  total.list[[j]] <- data_frame(disease= title,symptoms=paste(symptoms.newline,collapse = ","))
}


total.data.frame <- bind_rows(total.list)
write_csv(total.data.frame,"disease_symptoms.csv")

