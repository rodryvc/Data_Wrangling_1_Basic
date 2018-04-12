library(dplyr)
library(tidyr)
library(DataCombine)
library(psych)

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


################
## Alternative ##
################

Clean up brand names
refine$company <- gsub(".*ps$", "phillips", refine$company, ignore.case = TRUE); 
refine$company <- gsub("^a.*", "akzo", refine$company, ignore.case = TRUE); 
refine$company <- gsub("^v.*", "van houten", refine$company, ignore.case = TRUE); 
refine$company <- gsub("^u.*", "unilever", refine$company, ignore.case = TRUE);


data <- refine %>%
  separate(Product.code...number, c("product", "code"), sep = "-")
#dplyr does not knwo new column names

#Make sure to name column. 
data <- data %>% 
  mutate( product = case_when(product == "p" ~ "Smarthphone", 
                              product == "v" ~ "TV", 
                              product == "x" ~ "Laptop", 
                              product == "q" ~ "Tablet"))

# Create a new column full_address that concatenates the 
# three address fields (address, city, country), separated by commas.
data <- data %>%
  unite(full_address, address, city, country)

data <- data %>% 
  arrange(company, product)

##Create dummy variables for company and product category
#Both the company name and product category are categorical variables i.e. they take only a fixed set of values. 
#Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
#Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
#Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet

dummy <- dummy.code(data$company)
refinetemp <- data.frame(data, dummy)
colnames(refinetemp)[6:9] <- paste("company_", colnames(refinetemp[ c(6:9)]), sep = "")

dummy2 <- dummy.code(refinetemp$product)
refinal <- data.frame(refinetemp, dummy2)
colnames(refinal)[10:13] <- paste("product_", colnames(refinal[ c(10:13)]), sep = "")

write_csv(refinal, "refine_new.csv")




