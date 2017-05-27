# Data Wrangling Exercise 1

##Load libraries
library(dplyr)
library(dummies)
library(readr)
library(stringr)
library(tidyr)

#read file
refine.original = read_csv(file="refine_original.csv")
str(refine.original)

#convert all company names to lowercase
refine.original <- refine.original %>% mutate(company = tolower(company))
head(refine.original)

#check for distinct brand names
refine.original %>% distinct(company)

#clean up brand names
refine.original$company <- str_replace_all(refine.original$company, "phillips | phllips | phillps | fillips | phlips", "philips")
refine.original$company <- str_replace_all(refine.original$company, "akz0 | ak zo", "akzo")
refine.original$company <- str_replace_all(refine.original$company, "unilver", "unilever")

#seperate product code and product number
refine.clean <- separate(refine.original, col="Product code / number", c("product_code", "product_number"), sep="-", remove = TRUE)
head(refine.clean)

#add product category
fun.product.category <- function(product.code){
    product.code <- tolower(product.code)
    if( product.code == "p"){
    return("SmartPhone")
    } else if(product.code == "v"){
      return("TV")
  }
  else if(product.code == "x"){
    return("Laptop")
  }else if(product.code == "q"){
    return("Tablet")
  }
}

refine.clean <- refine.clean %>% mutate( product_category = sapply(product_code, fun.product.category))
str(refine.clean)

# add full address for geocoding
refine.clean <- unite(refine.clean, full_address, address:country, sep = ",")
str(refine.clean)

#create dummy variables for company and product category
refine.clean <- mutate(refine.clean, company_philips = 1*(company == "philips"))
refine.clean <- mutate(refine.clean, company_akzo = 1*(company == "akzo"))
refine.clean <- mutate(refine.clean, company_van_houten = 1*(company == "van houten"))
refine.clean <- mutate(refine.clean, company_unilever = 1*(company == "unilever"))
refine.clean <- mutate(refine.clean, product_smartphone = 1*(company == "smartphone"))
refine.clean <- mutate(refine.clean, product_tv = 1*(company == "TV"))
refine.clean <- mutate(refine.clean, product_laptop = 1*(company == "laptop"))
refine.clean <- mutate(refine.clean, product_tablet = 1*(company == "tablet"))

#write cleaned dataframe to csv file
write.csv(refine.clean, file = "refine_clean.csv")
