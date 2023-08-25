library(stringr)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
library(textstem)

#Infermedica
url <- "https://developer.infermedica.com/docs/available-symptoms"
rawhtml <- read_html(url)
raw.symtom.list <- rawhtml %>% html_node("body") %>% html_nodes("ul")%>%
  html_text(trim = TRUE)

symptoms.infermedica <- list()
l <- 1

for(j in 4:28){
  stlitstr <- raw.symtom.list[[j]] %>% strsplit(.,split = "\\r\\n")
  for (i in 1:length(stlitstr[[1]])) {
    symptoms.infermedica [l] <- gsub("[^a-zA-Z[:blank:]]","",stlitstr[[1]][i])
    symptoms.infermedica [l] <- substr(symptoms.infermedica [l],1,
                                       nchar(symptoms.infermedica [l]) - 1) %>% trimws()
    l <- l + 1
  }
}

symptoms.infermedica [1]

#csv
data <- read.csv("disease_symptom_dictionary.csv")
symptom.csv <- list()
l <- 1
for(i in 149:length(data$name)) {
  symptom.csv[l] <- as.character(data$name[i]) 
  l <- l + 1
}
 
#ada
data <- read.csv("reducediseasesymptoms.csv")
symptom.ada <- list()
l <- 1
for(i in 1:length(data$symptoms)) {
  val.raw <- as.character(data$symptoms[i])
  val.split <- strsplit(val.raw,split = ",")
  for(j in 1:length(val.split[[1]])) {
    symptom.ada[l] <- val.split[[1]][j] %>% trimws()
    l <- l + 1
  }
  
}

test <- as.vector(symptom.ada)
typeof(test)
v <- c('sweating increased','diziness','tenderness heal')
symptom.ada.stem <- stem_strings(unlist(symptom.ada))
symptom.csv.stem <- stem_strings(unlist(symptom.csv))
symptoms.infermedica.stem <- stem_strings(unlist(symptoms.infermedica))

symptom.stem <- c(symptom.ada.stem,symptom.csv.stem,symptoms.infermedica.stem)
symptom.names <- c(symptom.ada,symptom.csv,symptoms.infermedica)

#write symptom stem values to csv file
symptom.stem.dataframe <- data.frame(id = 1:1938,name = symptom.stem)
write.csv(symptom.stem.dataframe,"symptom_root.csv")

symptom.names.unlist <- unlist(symptom.names)

symptom.stem.unlist <- unlist(symptom.stem)

symptom.stem.mapping <- data.frame(name = unlist(symptom.names),
                                   root = unlist(symptom.stem))
write.csv(symptom.stem.mapping,"symptom_root_mapping.csv")
