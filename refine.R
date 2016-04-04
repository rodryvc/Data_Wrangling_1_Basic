library(dplyr)
library(tidyr)
library(DataCombine)
##Load data
prod_purch <- read.csv("refine_original.csv")
prod_purch

##Clean up brand names
prod_purch$company <- tolower(prod_purch$company)

#Replace incorrect values with FindReplace
ReplacesPhillips <- data.frame(from = c("philips", "fillips", "phlips", "phllips", "phillps"), to = c("phillips"))
prod_purch_clean <- FindReplace(data = prod_purch, Var = "company", replaceData = ReplacesPhillips, from = "from", to = "to", exact = FALSE)

ReplacesAkzo <- data.frame(from = c("akz0", "ak zo"), to = c("akzo"))
prod_purch_clean <- FindReplace(data = prod_purch_clean, Var = "company", replaceData = ReplacesAkzo, from = "from", to = "to", exact = FALSE)

ReplacesUnilever <- data.frame(from = c("unilver"), to = c("unilever"))
prod_purch_clean <- FindReplace(data = prod_purch_clean, Var = "company", replaceData = ReplacesUnilever, from = "from", to = "to", exact = FALSE)

prod_purch_clean 


##Separate Code and number
new_prod_purch <- separate(prod_purch_clean, col = Product.code...number, into =  c("product_code","product_name"))
new_prod_purch

##Add Product Category
list_prod_category <- c("p" = "Smartphone", "v" = "TV", "x" = "Laptop", "q" = "Tablet")
prod_category <- mutate(new_prod_purch, product_category = list_prod_category[new_prod_purch$product_code])
prod_category

##Add full address for geocoding
prod_category_with_fulladdress <- unite(prod_category, col = full_address, into = address, city, country, sep = ",")
prod_category_with_fulladdress

write.csv(prod_category_with_fulladdress, file = "refine_clean.csv")


