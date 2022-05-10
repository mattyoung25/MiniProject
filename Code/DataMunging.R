##################
# Data Munging
##################

### Load libraries
library(tidyverse)
library(ggplot2)

### Read in .csv file
df = read.csv("Data/Dataset_Contract_Sub-Awards.csv", stringsAsFactors = TRUE, na.strings=c("","NA"))

### Explore the dataset
summary(df)

### Keep variables that have differentiable elements
keep = c("subawardee_name", "prime_award_awarding_sub_agency_name", "prime_award_awarding_office_code", "subaward_fsrs_report_year","subaward_fsrs_report_month", "subaward_number", "subaward_amount", "subaward_action_date", "subaward_action_date_fiscal_year", "subawardee_duns","subawardee_dba_name", "subawardee_parent_duns", "subawardee_parent_name","subawardee_address_line_1", "subawardee_city_name", "subawardee_state_code", "subawardee_state_name", "subawardee_zip_code", "subawardee_congressional_district", "subawardee_congressional_district", "subawardee_business_types", "subaward_primary_place_of_performance_city_name", "subaward_primary_place_of_performance_state_code", "subaward_primary_place_of_performance_state_name","subaward_description","subawardee_highly_compensated_officer_1_name", "subawardee_highly_compensated_officer_1_amount", "subawardee_highly_compensated_officer_2_name", "subawardee_highly_compensated_officer_2_amount", "subawardee_highly_compensated_officer_3_name", "subawardee_highly_compensated_officer_4_name", "subawardee_highly_compensated_officer_5_name")

df = df %>% select(all_of(keep))

### Clean up subawardee name
df$subawardee_name = as.character(df$subawardee_name)
df$subawardee_name = gsub("INCORPORATED", "", df$subawardee_name)
df$subawardee_name = gsub(",", "", df$subawardee_name)
df$subawardee_name = gsub(" INC.", "", df$subawardee_name)
df$subawardee_name = gsub(" INC", "", df$subawardee_name)
df$subawardee_name = gsub(" LLC", "", df$subawardee_name)
df$subawardee_name = gsub(" L.L.C.", "", df$subawardee_name)
df$subawardee_name = gsub("DYNETICS TECHNICAL SOLUTIONS", "DYNETICS", df$subawardee_name)
df$subawardee_name = as.factor(df$subawardee_name)

### Clean up business type to transactions
df = df %>% separate(col = subawardee_business_types, into = c("business_type1", "business_type2", "business_type3", "business_type4", "business_type5", "business_type6", "business_type7", "business_type8", "business_type9"), sep = ",")
businesses = c("business_type1", "business_type2", "business_type3", "business_type4", "business_type5", "business_type6", "business_type7", "business_type8", "business_type9")
businessdf = df %>% select(all_of(businesses))

businessdf$label <- df$subawardee_name
### Clean up description
df$subaward_description = as.character(df$subaward_description)
df$subaward_description = gsub("&", " ", df$subaward_description)
df$subaward_description = gsub(",", "", df$subaward_description)
df$subaward_description = gsub("-", "", df$subaward_description)
df$subaward_description = gsub("  ", " ", df$subaward_description)
### Because most every category is NRE Engineering, it is being removed to differentiate variables
df$subaward_description = gsub(" NRE Other ", "", df$subaward_description)
df$subaward_description = gsub(" NRE Other", "", df$subaward_description)
df$subaward_description = gsub("NRE Other ", "", df$subaward_description)
df$subaward_description = gsub("NRE Other", "", df$subaward_description)
df$subaward_description = gsub(" NRE Engineering ", "", df$subaward_description)
df$subaward_description = gsub(" NRE Engineering", "", df$subaward_description)
df$subaward_description = gsub("NRE Engineering ", "", df$subaward_description)
df$subaward_description = gsub("NRE Engineering", "", df$subaward_description)
df$subaward_description = gsub(".", "", df$subaward_description, fixed=TRUE)
### Additionally remove pricing methods
df$subaward_description = gsub(" Firm Fixed Price ", "", df$subaward_description)
df$subaward_description = gsub(" Firm Fixed Price", "", df$subaward_description)
df$subaward_description = gsub("Firm Fixed Price ", "", df$subaward_description)
df$subaward_description = gsub("Firm Fixed Price", "", df$subaward_description)
df$subaward_description = gsub(" Cost Plus Fixed Fee ", "", df$subaward_description)
df$subaward_description = gsub(" Cost Plus Fixed Fee", "", df$subaward_description)
df$subaward_description = gsub("Cost Plus Fixed Fee ", "", df$subaward_description)
df$subaward_description = gsub("Cost Plus Fixed Fee", "", df$subaward_description)
df$subaward_description = as.factor(df$subaward_description)

### Chorpleth dataframe
chloro = df %>% select(subawardee_state_name, subaward_amount, subawardee_state_code)
chloro = chloro %>% group_by(subawardee_state_name, subawardee_state_code) %>% add_count() %>% summarize_all(sum)
colnames(chloro) <- c("state", "code", "value", "count")

### Bubbleplot dataframe
#### Create regions https://stackoverflow.com/questions/46066974/add-column-to-label-u-s-states-by-their-u-s-census-region
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

df$regions <- sapply(df$subawardee_state_code, 
                     function(x) names(region.list)[grep(x,region.list)])

df$regions <- as.factor(df$regions)


### Looking into business descriptions in regions
descriptiondf = as.data.frame(table(df$subaward_description, df$regions))
names(descriptiondf) <- c("Description", "region", "Freq")
descriptiondf = descriptiondf %>% filter(Description !="", Freq > 2)
Southdesc = descriptiondf %>% filter(region == "South")
Northeastdesc = descriptiondf %>% filter(region == "Northeast")
Midwestdesc = descriptiondf %>% filter(region == "Midwest")
Westdesc = descriptiondf %>% filter(region == "West")

### Looking into the key businesses in each region
businessnamedf = as.data.frame(table(df$subawardee_name, df$regions))
names(businessnamedf) <- c("CompanyName", "Region", "Contracts")
businessnamedf = businessnamedf %>% filter(Contracts > 3)

### Write dataframes to csv
write.csv(businessdf, "Data/businesstypestransactions.csv", row.names = F)
write.csv(chloro, "Data/chloropleth.csv", row.names = F)
write.csv(descriptiondf, "Data/businessdescription.csv", row.names = F)
write.csv(businessnamedf, "Data/businessnames.csv", row.names = F)

