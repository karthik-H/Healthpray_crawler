library(dplyr)
symptom.disease.mapping <- file
symptom.code.map <- read.csv("newdataset/symptoms_code_mapping_original.csv")
disease.code.map <- read.csv("newdataset/disease_code_mapping_original.csv")

min.Tfid <- 5
unique.disease <- unique(symptom.disease.mapping$MeSH.Disease.Term) %>% tolower(.) 
total.disease <- tolower(symptom.disease.mapping$MeSH.Disease.Term)
disease.symptoms <- vector("list")

#training data by code
for(j in 1:length(unique.disease)) {
  print(paste("disease name",unique.disease[j],sep = " : "))
  index.list <- which(total.disease %in% unique.disease[j])
  symptoms <- ""
  for(i in index.list) {
    if(symptom.disease.mapping$TFIDF.score[i] > min.Tfid) {
      symptoms <- paste(symptoms,symptom.code.map$code[match(
                                        tolower(symptom.disease.mapping$MeSH.Symptom.Term[i]),
                                                             symptom.code.map$name)],sep = ",")
    }
  }
  disease.symptoms[[j]] <- data_frame(disease = disease.code.map$code[
                                      match(trimws(unique.disease[j]),disease.code.map$name)],
                                                                  symptoms = symptoms)
}
data.frame.ds <- bind_rows(disease.symptoms)
write.csv(data.frame.ds,"disease_symptoms_mapping_by_code.csv")
data.frame.ds$symptoms[1]


#
#
#training data by name
disease.symptoms <- vector("list")
data.frame.ds <- data.frame()
min.Tfid <- 5
for(j in 1:5) {
  print(paste("disease name",unique.disease[j],sep = " : "))
  index.list <- which(total.disease %in% unique.disease[j])
  symptoms <- ""
  for(i in index.list) {
    if(symptom.disease.mapping$TFIDF.score[i] > min.Tfid) {
      symptoms <- paste(symptoms,symptom.disease.mapping$MeSH.Symptom.Term[i],sep = ",")
    }
  }
  disease.symptoms[[j]] <- data_frame(disease = trimws(unique.disease[j]),symptoms = symptoms)
}



data.frame.ds <- bind_rows(disease.symptoms)
data.frame.ds$symptoms
write.csv(data.frame.ds,"disease_symptoms_mapping_by_name.csv")
symptoms



symptom.code.map$code[match(
  tolower(symptom.disease.mapping$MeSH.Symptom.Term[1]),
  symptom.code.map$name)]

symptom.disease.mapping$MeSH.Symptom.Term[1]












file.old <- read.csv('disease_symptom_dictionary.csv')
old.symptom <- file.old$name
old.symptom <- old.symptom[1:150]
old.symptom.lower <- tolower(old.symptom)
unique.disease.lower <- tolower(unique.disease)
which(old.symptom.lower %in% unique.disease.lower)


disease.code.map$code[match(unique.disease[j],disease.code.map$name)]



#code 
#disease symptom code
Mesh.Symptom.Tesm <- unique(symptom.disease.mapping$MeSH.Symptom.Term)

unique.symptom.lower <- unique(Mesh.Symptom.Tesm) %>% tolower(.)
unique.disease.lower

#code data
symptom.code <- read.csv('symptom_code.csv')
name.data <- symptom.code$DescriptorName
code.data <- symptom.code$DescriptorUI
name.data.lower <- tolower(name.data)

name.data.lower <- gsub(",","-",name.data.lower)
global_count <- 1

new.name <- vector("list")
for(i in 1:length(unique.disease.lower)) {
  print("...")
  if(is.na(match(unique.disease.lower[i],name.data.lower))) {
    new.name[[i]] <- data_frame(name = unique.disease.lower[i],
                                code = new.data.frame.disease$code[
                                  match(unique.disease.lower[i],
                                        new.data.frame.disease$name)])
    
    
  }
  else new.name[[i]] <- data_frame(name = unique.disease.lower[i],
                                          code = symptom.code$DescriptorUI[
                                            match(unique.disease.lower[i],
                                                  name.data.lower)])
}

disease.code.map <- bind_rows(new.name)
write.csv(disease.code.map,"disease_code_mapping_original.csv")
#new code map
symptom.code.map
disease.code.map
#new data
new.data.frame.disease
new.data.frame.symptom 


