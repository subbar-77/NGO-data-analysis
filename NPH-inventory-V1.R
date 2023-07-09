rm(list = ls())

#install.packages("GGally", dependencies = T)

library(data.table)
library(openxlsx)
library(imputeTS)
library(tidyverse)
library(dplyr)
library(readxl)
library(gbm)
library(GGally) #for correlation cofefficient graphs
library(caret)
library(psych)

inven_19 <- read_excel("Inventory19-Edited.xlsx")
citizen  = read_excel("Population-italy-wiki.xlsx")
gdp = read_excel('GDP italy-wiki.xlsx')
pharma_19 = read_excel('Pharmacies  Report-edited.xlsx')
volun_19 = read_excel('Volunteers Report-edited.xlsx')
orgs_19 = read_excel('Partnered Orgs Report-edited.xlsx')
prop_region = fread('./propensity_to_donate_region.csv', header = T)
confezioni_region = fread('./confezioni_GRF_regions.csv')
distribution_region = fread('./total collection and pharmacies available_distribution.csv')
lomb_city_demo = fread('./Lombardia.csv')

summary(inven_19)
#inven_19$Time_stamp = as.Date(inven_19$Time_stamp, format = c('%d/%m/%Y %h.%m.%s'))
names(inven_19) = toupper(names(inven_19))

#Dropping the columns
inven_19$YOU<-NULL
inven_19$PHONE <- NULL
inven_19$MAIL<-NULL
inven_19$NEXT_YR_PARTICPATION = NULL
inven_19$NOTES = NULL
inven_19$EST_ECONOMIC_VALUE = NULL

#Data cleaning -----------------------------------------------

#Formatting the names of province, region and city
inven_19$REGION = toupper(inven_19$REGION)
unique(inven_19$REGION)
uniqueN(inven_19$REGION) #20 regions
sum(is.na(inven_19$REGION)) #no NA

inven_19$PROVINCE = toupper(inven_19$PROVINCE)
unique(inven_19$PROVINCE)
uniqueN(inven_19$PROVINCE) #105 provinces
sum(is.na(inven_19$PROVINCE)) #no NA
prov_inven = unique(inven_19$PROVINCE)

inven_19$CITY = toupper(inven_19$CITY)
unique(inven_19$CITY)
uniqueN(inven_19$CITY) #886 cities
sum(is.na(inven_19$CITY)) #no NA

summary(inven_19$TOTAL)
sapply(inven_19,class) #-> eye drops, others, est economic value, participation years is saved as a character

inven_19$`EYE DROPS` = as.numeric(inven_19$`EYE DROPS`) # converting number of eye drops to numbers

#Replacing 34 NA in NASAL_ASPIRATORS with 0
inven_19$NASAL_ASPIRATORS= na_replace(inven_19$NASAL_ASPIRATORS,0)

#Replacing 34 NA in DRINKS with 0
inven_19$DRINKS= na_replace(inven_19$DRINKS,0)

#Replacing 315 NA in EST-Economiv_value with 0, (dropped column)
# inven_19$EST_ECONOMIC_VALUE= na_replace(inven_19$EST_ECONOMIC_VALUE,0) #from imputeTS
# as.numeric(inven_19$EST_ECONOMIC_VALUE)
# sum(is.na(inven_19$EST_ECONOMIC_VALUE))  #no NA values

#Replacing 145 NA in OTHERS with 0
inven_19$OTHERS = as.numeric(inven_19$OTHERS)
inven_19$OTHERS= na_replace(inven_19$OTHERS,0) #from imputeTS
sum(is.na(inven_19$OTHERS))  #no NA values

#320 rows doesnt have participation years, (ASSUMING THAT THIS IS THEIR FIRST YEAR, SO PARTICIPATION_YEARS = 0)
inven_19$PARTICIPATION_YEARS =  as.numeric(inven_19$PARTICIPATION_YEARS, na.rm=T)
inven_19$PARTICIPATION_YEARS= na_replace(inven_19$PARTICIPATION_YEARS,0) #from imputeTS
sum(is.na(inven_19$PARTICIPATION_YEARS))  #no NA values

inven_19$OTHERS = as.numeric(str_extract(inven_19$OTHERS, "[0-9]+")) #extracting numbers from others column, library 'stringr'
inven_19$OTHERS= na_replace(inven_19$OTHERS,0) #from imputeTS
sum(inven_19$OTHERS)

#replacing missing feedback with mean feedback rating
inven_19$INSTRUCTIONS_FEEDBACK = na_replace(inven_19$INSTRUCTIONS_FEEDBACK, mean(inven_19$INSTRUCTIONS_FEEDBACK,na.rm = T))
sum(is.na(inven_19$INSTRUCTIONS_FEEDBACK))

#replacing missing total value with sum of columns
inven_19$TOTAL = na_replace(inven_19$TOTAL,rowSums(inven_19[,6:28]))

inven_19$TOTAL_CALC = rowSums(inven_19[,6:28])
inven_19$total = NULL

inven_prov_list = unique(inven_19$PROVINCE)

hist(inven_19$TOTAL_CALC)
summary(inven_19$TOTAL_CALC)

#population dataset-----------------------------------
names(citizen) = toupper(names(citizen))
citizen$PROVINCE = toupper(citizen$PROVINCE)
citizen$ADMINISTRATIVE_REGION = toupper(citizen$ADMINISTRATIVE_REGION)
#ISO_CODE is the code of the province

uniqueN(citizen$ISO_CODE) #107 provinces
prov_citi = unique(citizen$ISO_CODE)
prov_inven[!prov_inven %in% prov_citi] #OT has been abolished as a province, 
                                        #the only value that we dont have population
colSums(is.na(citizen))
mini_citizen = citizen[,c("PROVINCE","ISO_CODE", "POPULATION", "MACRO_REGION", "AREA(KM2)", "DENSITY(/KM²)")]
inven_pop = merge(inven_19, mini_citizen, by.x = 'PROVINCE', by.y = 'ISO_CODE', all.x = T)
colSums(is.na(inven_pop))

#dropping one column of OT as the data isnt available
inven_pop = drop_na(inven_pop)

#GDP dataset
names(gdp) = toupper(names(gdp))
gdp$REGION = toupper(gdp$REGION)
gdp$count = 1
gdp_prov = gdp[,c("PROVINCE","2015_GDP_(MIL_EURO)")]
gdp_prov_list = unique(gdp$PROVINCE)

#volunteers
volun_19$count=1
volun_prov = aggregate(volun_19$count, list(volun_19$PR), sum)
names(volun_prov) = list('PROVINCE','VOLUNTEERS_COUNT')

#PHARMACIES
pharma_19$count = 1
pharma_prov = aggregate(pharma_19$count, list(pharma_19$PR), sum)
names(pharma_prov) = list('PROVINCE', 'PHARMA_COUNT')

#PARTNERED ORGANIZATIONS
orgs_19$count = 1
orgs_prov = aggregate(orgs_19$count, list(orgs_19$PR), sum)
names(orgs_prov) = list('PROVINCE','ORGS_COUNT')

#COnfezioni
confezioni_region$`% farmacie sul totale` = str_replace(confezioni_region$`% farmacie sul totale`,',','.')
confezioni_region$`% farmaci sul totale` = sub(',','.',confezioni_region$`% farmaci sul totale`)
#both the abbove commands replace ',' as decimal seperator to '.'
confezioni_regions = unique(confezioni_region$`0`)
confezioni_region$REGION = toupper(confezioni_region$REGION)
base::names(confezioni_region) = c('REGION','Total_Pharma_in_Region',
                                '%total_pharma_region','Pharma_Donated_Similar_Initiative_Same_Year',
                                '%DRUGS_out_of_total','avg_Donations_per_pharma')

#propernsity to donate:
prop_region$REGION = toupper(prop_region$REGION)
names(prop_region) = c('REGION','PROPENSITY_TO_DONATE_2018','PROPENSITY_TO_DONATE_2019')

#distribustion analysis:
distribution_region$REGION = toupper(distribution_region$REGION)

#Lombardia demographics Dataset:
names(lomb_city_demo) = toupper(names(lomb_city_demo))
lomb_city_demo$NAME = toupper(lomb_city_demo$NAME)

#Region analysis--------------------------------------
region = inven_pop[,c('REGION','TOTAL_CALC',"POPULATION","AREA(KM2)")]
region$COUNT = 1

#region_inven[,.(new := aggregate(inven_pop$TOTAL_CALC, by = list(inven_pop$REGION), sum))]
#population = aggregate(inven_pop$POPULATION, by = list(inven_pop$REGION),sum)
region_inven = region %>% group_by(REGION) %>% 
                    summarise(total_collected=sum(TOTAL_CALC), 
                              # population=sum(POPULATION),
                              # total_area = sum(`AREA(KM2)`),
                              total_pharma = sum(COUNT))
region_pop = aggregate(citizen$POPULATION, list(citizen$ADMINISTRATIVE_REGION), sum)
names(region_pop) = list('REGION', 'POPULATION')
region_area = aggregate(citizen$`AREA(KM2)`, list(citizen$ADMINISTRATIVE_REGION), sum)
names(region_area) = list('REGION', 'AREA')
region_inven = merge(region_inven, merge(region_area,region_pop) ,all.x = T, all.y = T)

region_gdp = aggregate(gdp$`2015_GDP_(MIL_EURO)`,list(gdp$REGION), sum)
names(region_gdp) = list('REGION',"GDP")

region_inven = merge(region_inven, region_gdp, by = 'REGION', all.x = T, all.y = T)
#above line is to create a pivot table using pipe function
region_inven$coll_per_person = round(region_inven$total_collected/region_inven$POPULATION,5)
region_inven$coll_per_area = round(region_inven$total_collected/region_inven$AREA,3)
region_inven$coll_per_pharma = round(region_inven$total_collected/region_inven$total_pharma,3)
region_inven$'pharmas_per_mil_ppl' = round(region_inven$total_pharma/region_inven$POPULATION*1000000,2)
region_inven$pharma_per_km2 = round(region_inven$total_pharma/region_inven$AREA,4)
region_inven$DENSITY = round(region_inven$POPULATION/region_inven$AREA,3)
region_inven = arrange(region_inven,-total_collected)

# LOMBARDIA, VENETO & EMILIA ROMAGNA are the TOP three regions and collects 52.42% of total products
# VALLE D'AOSTA, MOLISE & TRENT ALTO ADIGE are the BOTTOM three regions & collects 0.88% of total
region_inven$percent_collected = round(region_inven$total_collected/(sum(region_inven$total_collected))*100,2)
regions_inven = unique(inven_19$REGION)
region_inven = merge(region_inven, prop_region, all.x = T)
region_inven$PROPENSITY_TO_DONATE_2018 = NULL
plot(region_inven$percent)
write.xlsx(region_inven, file = 'B:/Term 2/AI-2/NPH Italy-project/NPH/region_summary.xlsx',
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


#Model Data---------
model_data_1 = inven_19
model_data_1[,6:29] = NULL
model_data_1$TIME_STAMP = NULL
#model_data_1$PROVINCE.y = NULL
#model_data_1$CITY = NULL
model_data_1 = merge(model_data_1, citizen, by.x = 'PROVINCE', by.y = 'ISO_CODE', all.x = T)
model_data_1$ADMINISTRATIVE_REGION = NULL
model_data_1$PROVINCE.y = NULL
model_data_1[16:17] = NULL
model_data_1$CAPITAL = NULL
model_data_1$COMUNI = NULL
model_data_1$PRESIDENT = NULL
model_data_1$TYPE = NULL
model_data_1 = drop_na(model_data_1)
model_data_1$REGION = as.factor(model_data_1$REGION)
#model_data_1$TYPE = as.factor(model_data_1$TYPE)
model_data_1$MACRO_REGION = as.factor(model_data_1$MACRO_REGION)
model_data_1$count = 1
model_data_1 = merge(model_data_1, merge(volun_prov, pharma_prov, all.x = T, all.y = T), 
                     all.x = T)

model_data_1 = merge(model_data_1, merge(orgs_prov,gdp_prov, all.x = T, all.y = T
                                        ), all.x = T)
model_data_1$VOLUNTEERS_COUNT = na_replace(model_data_1$VOLUNTEERS_COUNT,0)
model_data_1$PHARMA_COUNT = na_replace(model_data_1$PHARMA_COUNT,1)
model_data_1$ORGS_COUNT = na_replace(model_data_1$ORGS_COUNT, 0)

#Derived variables
model_data_1$COLL_PER_MILLION_PERSON = round((model_data_1$TOTAL_CALC/model_data_1$POPULATION)*1000000,3)
model_data_1$COLL_PER_AREA = round(model_data_1$TOTAL_CALC/model_data_1$`AREA(KM2)`,3)
model_data_1$COLL_PER_PHARMA = round(model_data_1$TOTAL_CALC/model_data_1$PHARMA_COUNT,3)
model_data_1$PHARMA_PER_MILLION_PEOPLE = round((model_data_1$PHARMA_COUNT/model_data_1$POPULATION)*1000000,3)
model_data_1$PHARMA_PER_100_KM2 = round((model_data_1$PHARMA_COUNT/model_data_1$`AREA(KM2)`)*100,5)
model_data_1 = merge(model_data_1,merge(prop_region,confezioni_region, all.x = T,
                                        all.y = T, by.x = 'REGION', by.y = 'REGION')
                     , all.x = T)
#model_data_1 = merge(model_data_1,distribution_region, all.x = T)
model_data_1$PROPENSITY_TO_DONATE_2018 = NULL
model_data_1$`%total_pharma_region` = NULL
model_data_1$`%DRUGS_out_of_total` = NULL
model_data_1$avg_Donations_per_pharma = NULL
model_data_1$Pharma_Donated_Similar_Initiative_Same_Year = NULL
model_data_1$count = NULL

#correlation coefficient graphs
ggpairs(model_data_1[,c("POPULATION","DENSITY(/KM²)","AREA(KM2)",
                        "COLL_PER_MILLION_PERSON","COLL_PER_AREA",
                        "COLL_PER_PHARMA","PHARMA_PER_MILLION_PEOPLE",
                        "PHARMA_COUNT")])

write.xlsx(model_data_2, file = 'B:/Term 2/AI-2/NPH Italy-project/NPH/merged.xlsx',
          sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

model_data_2 = model_data_1
model_data_1[,1:4] = NULL
model_data_1$MACRO_REGION = NULL

model1 = glm(TOTAL_CALC~.-`2015_GDP_(MIL_EURO)`-PHARMA_PER_100_KM2, data = model_data_1)
summary(model1)

opt_model1 = step(model1)
summary(opt_model1)

model2 = gbm(TOTAL_CALC ~ .-`2015_GDP_(MIL_EURO)`-PHARMA_PER_100_KM2-PARTICIPATION_YEARS, data = model_data_1, 
             n.trees = 1000, shrinkage = 0.01, train.fraction = 0.75)
summary(model2)

model3 = gbm(TOTAL_CALC~`AREA(KM2)`+POPULATION+PHARMA_COUNT+INSTRUCTIONS_FEEDBACK+`DENSITY(/KM²)`
             +VOLUNTEERS_COUNT+ORGS_COUNT+PROPENSITY_TO_DONATE_2019+Total_Pharma_in_Region, data = model_data_1, 
             n.trees = 1000, shrinkage = 0.01, train.fraction = 0.75)
summary(model3)

model4 = glm(TOTAL_CALC~`AREA(KM2)`+POPULATION+PHARMA_COUNT+INSTRUCTIONS_FEEDBACK+`DENSITY(/KM²)`
             +VOLUNTEERS_COUNT+ORGS_COUNT+PROPENSITY_TO_DONATE_2019+Total_Pharma_in_Region, data = model_data_1)
summary(model4)

model5 = step(model4)

#Caret implementation-----

inTrain <- createDataPartition(y = model_data_1$TOTAL_CALC,
                               p = 0.75,
                               list = F)
train <- model_data_1[inTrain,]
test <- model_data_1[-inTrain,]

data_ctrl <- trainControl(method = "cv", number = 5)

gbm_grid <- expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (0:50) * 50,
                        shrinkage = c(0.01, 0.001),
                        n.minobsinnode = 10)

gbm_caret <- train(TOTAL_CALC ~ .-`2015_GDP_(MIL_EURO)`-PHARMA_PER_100_KM2,
                   data = model_data_1,
                   trControl = data_ctrl,
                   # preProc = c("center", "scale"),
                   method = "gbm",
                   tuneGrid = gbm_grid,
                   metric = 'RMSE')

gbm_caret$finalModel
gbm_caret$resample
gbm_caret_results <- gbm_caret[['results']]
ggplot(gbm_caret)

predictions_train <- predict(object = gbm_caret,
                             type = 'raw',
                             newdata = train)
RMSE(model_data_1$TOTAL_CALC, predictions_train)

predictions_test <- predict(object = gbm_caret,
                            type = 'raw',
                            newdata = test)
RMSE(test$TOTAL_CALC, predictions_test)


model_data_2$predictions = predict(object = gbm_caret,
                                   type = 'raw',
                                   newdata = model_data_2)
model_data_2$delta = model_data_2$TOTAL_CALC - model_data_2$predictions
gbm_caret_prov = aggregate(model_data_2$delta, list(model_data_2$REGION,model_data_2$PROVINCE), sum )
names(gbm_caret_prov) = c('REGION', 'PROVINCE',  'ERROR')
gbm_caret_region = aggregate(gbm_caret_prov$ERROR,list(gbm_caret_prov$REGION),sum)
names(gbm_caret_region) = c('REGION', 'ERROR')

#Lombardia Analysis-----------
inven_lomb = inven_19[inven_19$REGION == 'LOMBARDIA',]
inven_lomb[,6:29] = NULL
inven_lomb$TIME_STAMP = NULL
names(inven_lomb)
inven_lomb_city = unique(inven_lomb$CITY)
#cannot merge as lot of cities names are missing

