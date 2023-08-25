#install.packages("Rcrawler")
library(Rcrawler)
#Rcrawler doesn't work on all websites if it works then it will download entire content
#of website to local drive
#Rcrawler(Website = "https://ada.com/careers/",no_cores = 2,no_conn = 2)

page <- LinkExtractor(url="https://www.apollohospitals.com/patient-care/health-and-lifestyle/diseases-and-conditions/")
page$InternalLinks
ada.condition <- list()
for(i in 5:348) {
  ada.condition [i - 4]<- page$InternalLinks[i]
}
print(ada.condition[1:5])
