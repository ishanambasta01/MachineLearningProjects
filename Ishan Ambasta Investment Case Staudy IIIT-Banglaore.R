#Ishan Ambasta IIIT-Bangalore
library(dplyr)
library(stringr)
library(tidyr)

#------------------------------------------Checkpoint 1: Data Cleaning 1----------------------------------

#Load the companies and rounds data into two data frames and name them companies and rounds2 respectively
companies <- read.delim("companies.txt", stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

##cleaning i.e. converting to upper case,the permalink column in companies and rounds2 dataframes
companies <- mutate(companies, permalink = str_to_upper(companies$permalink))

rounds2 <- mutate(rounds2, company_permalink = str_to_upper(rounds2$company_permalink))
rounds2$raised_amount_usd [is.na(rounds2$raised_amount_usd)]<-0

#---------------------------------------Table 1.1-------------------------------------------------------

#1.unique companies are present in rounds2
length(unique(rounds2$company_permalink))

#2.unique companies are present in the companies file
length(unique(companies$permalink))

#A3.re there any companies in the rounds2 file which are not present in companies
setdiff(companies$permalink, rounds2$company_permalink)

#5.Merge the two data frames so that all variables (columns) in the companies frame are added to 
  #the rounds2 data frame.Name the merged frame master_frame.
  #How many observations are present in master_frame

master_frame <- merge(rounds2,companies, by.x = "company_permalink", by.y = "permalink")

#--------------------------------------Checkpoint 2: Funding Type Analysis------------------------------

#--------------------------------------Table 2.1--------------------------------------------------------

#1,2,3,4.Average funding amount based on funding_round_type

master_frame %>%
  group_by(funding_round_type) %>%
  summarize(mean_Funding = mean(raised_amount_usd, na.rm = TRUE))

#5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
  #which investment type is the most suitable for them?

master_frame %>%
  group_by(funding_round_type) %>%
  summarize(mean_Funding = mean(raised_amount_usd, na.rm = TRUE))%>% 
  arrange(mean_Funding)


#-------------------------------------Checkpoint 3: Country Analysis------------------------------------

#install.packages(pdftools)
library(pdftools)

#Load Countries_where_English_is_an_official_language in a dataframe

Country_English_official_language <- pdf_text("Countries_where_English_is_an_official_language.pdf")

Country_English_official_language <- strsplit(Country_English_official_language,
                                              "                   |\r\n")
#cleaning data

list_country_english_official<-as.data.frame(trimws(Country_English_official_language[[1]], which = c("both", "left", "right")))
colnames(list_country_english_official)<-"country_list"

#removing continent and other headings

remove_values<-c("List of countries where English is an official language",
                 "Africa","Asia","Australia/Oceania","Europe","Americas","")

list_country_english_official<-as.data.frame(list_country_english_official[!(is.element(list_country_english_official$country_list, remove_values)),])
colnames(list_country_english_official)<-"country_list"

#----Append country codes with each country

#install.packages("countrycode")
library(countrycode)

list_country_english_official<-cbind(list_country_english_official,Country_code=countrycode(list_country_english_official$country_list,'country.name', 'iso3c'))

#----------------------------------------------------Table 3.1----------------------------

#For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)

top9<-(master_frame %>%
  filter(funding_round_type=="venture") %>%
  group_by(country_code) %>%
  summarize(Total_funding = sum(raised_amount_usd, na.rm = TRUE))%>%
  arrange(desc(Total_funding)))[1:9,]

#1,2,3. Top English speaking country
topEng<-merge(top9,list_country_english_official,by.x="country_code",by.y = "Country_code")
topEng<-topEng%>%
  arrange(desc(Total_funding))

#-----------------------------------------------------Checkpoint 4: Sector Analysis 1-------------------

# load mapping file 
mapping<- read.csv(file = "mapping.csv",stringsAsFactors = FALSE)

#Extract the primary sector of each category list from the category_list column

#--Expected Result: Code for a merged data frame with each primary sector mapped to its main sector (the primary sector should be present in a separate column).
mapping_primary_sector<-gather(mapping,key=primary_sector,value=mapping, 2:10)
mapping_primary_sector<-mapping_primary_sector[mapping_primary_sector$mapping==1,]

#--separate sectors from category list for columns containing more than one sectors in merge_frame dataframe
merge_master_dataframe<-separate(master_frame,category_list,c("category_list","secondary_category"),sep = "\\|",extra = "drop")
merge_master_dataframe$category_list[is.na(merge_master_dataframe$category_list)] <- ''
merge_master_dataframe$secondary_category[is.na(merge_master_dataframe$secondary_category)] <- ''

#--merge mapping and master_frame based on category_list
merge_master_dataframe<- merge(merge_master_dataframe,mapping_primary_sector,by = "category_list")
merge_master_dataframe<-merge_master_dataframe[,-18]

#------------------------------Checkpoint 5: Sector Analysis 2------------------------------------------------

Country1<-topEng$country_code[1]
Country2<-topEng$country_code[2]
Country3<-topEng$country_code[3]
FT<-"venture"

#Create three separate data frames D1, D2 and D3 for each of the three countries containing the 
  #observations of funding type FT falling within the 5-15 million USD range.

D1<-filter(merge_master_dataframe, funding_round_type == FT, country_code == Country1)
D2<-filter(merge_master_dataframe, funding_round_type == FT, country_code == Country2)
D3<-filter(merge_master_dataframe, funding_round_type == FT, country_code == Country3)

#------------------------------------Table 5.1 ---------------------------------------------------------

#1. Total number of Investments (count)
#---country1
count(D1)

#---country2
count(D2)

#---country3
count(D3)

#2Total amount of investment (USD)
#---country1
sum(D1$raised_amount_usd,na.rm = TRUE)

#---country2
sum(D2$raised_amount_usd,na.rm = TRUE)

#---country3
sum(D3$raised_amount_usd,na.rm = TRUE)

#--Top Sector name (no. of investment-wise) point 3-8 for country 1
D1%>%
  group_by(primary_sector) %>%
  summarise(count=n())%>%
  arrange(desc(count))

#---Top Sector name (no. of investment-wise) point 3-8 for country 2
D2%>%
  group_by(primary_sector) %>%
  summarise(count=n())%>%
  arrange(desc(count))

#--Top Sector name (no. of investment-wise) point 3-8 for country 3
D3%>%
  group_by(primary_sector) %>%
  summarise(count=n())%>%
  arrange(desc(count))

#9.For point 3 (top sector count-wise), which company received the highest investment?

#---country1
D1%>%
  filter(primary_sector=="Others")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#---country2
D2%>%
  filter(primary_sector=="Others")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#---country3
D3%>%
  filter(primary_sector=="Others")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#10.For point 4 (second best sector count-wise), which company received the highest investment?

#---country1
D1%>%
  filter(primary_sector=="Cleantech...Semiconductors")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#---country2
D2%>%
  filter(primary_sector=="Cleantech...Semiconductors")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#---country3
D3%>%
  filter(primary_sector=="News..Search.and.Messaging")%>%
  group_by(company_permalink)%>%
  summarise(Total_investment=sum(raised_amount_usd,na.rm = TRUE))%>%
  arrange(desc(Total_investment))

#-----------------------------------------------------Checkpoint 6: Plots----------------------------

#export data required to create chats in tableau

write.csv(master_frame,"merge_master.csv", row.names = FALSE)
write.csv(merge_master_dataframe,"merge_master_frame.csv", row.names = FALSE)
top9$country_code[is.na(top9$country_code)] <- ''
write.csv(top9,"top9.csv", row.names = FALSE)

list_country_english_official$country_list[is.na(list_country_english_official$country_list)] <- ''
write.csv(list_country_english_official,"officialEnglish.csv",row.names = FALSE)