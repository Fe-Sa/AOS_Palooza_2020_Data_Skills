## AOS Palooza data exploration script
## By Donal O'Leary of Battelle's National Ecological Observatory Network (NEON)
## For the 2020 AOS Palooza
## These materials are based on the NEONscience.org data skills tutorials
## and referenced where appropiriate.
## Full credit to the respective authors of those tuorials. 

# install.packages("neonUtilities")
# install.packages("tidyverse")

# load packages
library(neonUtilities)
library(tidyverse)
library(reshape2)

# download data product and load into environment
# for an excellent tutorial see: https://www.neonscience.org/download-explore-neon-data
h2o_chemistry_full=loadByProduct(dpID="DP1.20093.001",
                                 site="CRAM", package = 'expanded',check.size = F)
#loadByProduct() gives us a big, complex 'list', that we called h2o_chemistry_full

# To view the use guide for this product, go to:
# https://data.neonscience.org/documents/10179/2237401/NEON_waterChem_userGuide_vA/4974e1a3-1937-470c-ac29-061a54524bf0

# To make this easier to work with, we will use the list2env() function to give us the individual 
# list items as data.frames
list2env(h2o_chemistry_full, .GlobalEnv)

# Let's take a look at all of the variables available to us in this data product:
View(variables_20093)


# We can also see that there are two separate tables for the domainLabData 
# and the externalLabData, and they measure different things!
names(swc_domainLabData)
names(swc_externalLabData)


# extract the 'location' from the SampleID to use as a plotting variable:
swc_domainLabData$location=substr(swc_domainLabData$parentSampleID, 6,7)
swc_fieldSuperParent$location=substr(swc_fieldSuperParent$parentSampleID, 6,7)

## Make a plot showing ALK and ANC for different locations
ggplot(data=swc_domainLabData, mapping=aes(x=collectDate))+
  geom_line(data=swc_fieldSuperParent[swc_fieldSuperParent$location %in% c("C1","C0"),], mapping=aes(y=waterTemp),col="blue")+
  geom_point(mapping=aes(y=alkMgPerL,shape=sampleType, color=location))+
  geom_point(mapping=aes(y=ancMgPerL, shape=sampleType,color=location))+
  ylab("Mg per L, and Degrees C at water surface")+
  ggtitle("Domain Support Lab ANC, ALK, and Temp")


## Make plot comparing dissolved oxygen and conductivity
swc_fieldMelt=melt(swc_fieldSuperParent, id.vars=c("parentSampleID","collectDate","location"), measure.vars=c("dissolvedOxygen","specificConductance"))

ggplot(data=swc_fieldMelt[swc_fieldMelt$location %in% c("C1","C0","C2"),], mapping=aes(x=collectDate))+
  geom_line(data=swc_fieldSuperParent[swc_fieldSuperParent$location %in% c("C1","C0"),], mapping=aes(y=waterTemp),col="blue")+
  geom_point(mapping=aes(y=value, shape=location, color=variable))+
  geom_point(mapping=aes(y=value, shape=location, color=variable))+
  ylab("DO (pct%), Conductance (ms/ml), and Temp (*C)")+
  ggtitle("Field Data")


## extract only samples from same date
## because the latest samples haven't returned from the eternal lab yet

# select only the samples from the domainLab with dates that match the externalLab's dates
swc_domain_comparable=swc_domainLabData[swc_domainLabData$parentSampleID %in% swc_externalLabData$sampleID,]

# likewise, select only the external lab samples with daets that match 
swc_external_comparable=swc_externalLabData[swc_externalLabData$sampleID %in% swc_domainLabData$parentSampleID,]

## Make combined data.frame for alkalinity (bicarbonate)

domain_alk=filter(swc_domain_comparable, sampleType=="ALK")

alk_combined_df=full_join(x=domain_alk, y=swc_external_comparable, by=c("parentSampleID"="sampleID"))

alk_combined_df$location=substr(alk_combined_df$parentSampleID,6,7)

swc_combined=full_join(x=swc_domain_comparable, y=swc_external_comparable, by=c("parentSampleID"="sampleID"))

swc_combined$location=substr(swc_combined$parentSampleID,6,7)


#xlim(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-01")) #must convert date ranges to POSIXct to match input data

# plot a comparison of ALK values between domainLab and externalLab with 
ggplot(data=alk_combined_df, mapping=aes(x=alkMgPerL, y=waterBicarbonate, shape=location))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  #stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain ALK")+
  ylab("External lab bicarbonate")

# read the summary information for the linear model here
summary(lm(data=alk_combined_df, waterBicarbonate ~ alkMgPerL))

cor.test(alk_combined_df$waterBicarbonate,alk_combined_df$alkMgPerL)


## hmmmm, looks like that one outlier is really throwing things off.
## Let's remove it and try again

alk_combined_df=alk_combined_df[alk_combined_df$waterBicarbonate<50,]

# plot a comparison of ALK values between domainLab and externalLab with 
ggplot(data=alk_combined_df, mapping=aes(x=alkMgPerL, y=waterBicarbonate))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain ALK")+
  ylab("External lab bicarbonate")

# read the summary information for the linear model here
summary(lm(data=alk_combined_df, waterBicarbonate ~ alkMgPerL))

cor.test(alk_combined_df$waterBicarbonate,alk_combined_df$alkMgPerL)

## Wow, it still isn't very good! 
## Perhaps there are different titration styles? Or the alkalinity is so low that 
## the measurement uncertainty is large compared to the true value?

#######
# Compare conductivity

# select only the samples from the domainLab with dates that match the externalLab's dates
swc_domain_comparable=swc_fieldSuperParent[swc_fieldSuperParent$parentSampleID %in% swc_externalLabData$sampleID,]

# likewise, select only the external lab samples with daets that match 
swc_external_comparable=swc_externalLabData[swc_externalLabData$sampleID %in% swc_fieldSuperParent$parentSampleID,]

ggplot()+
  geom_point(swc_domain_comparable, mapping=aes(x=collectDate, y=specificConductance), shape=1)+
  geom_point(data=swc_external_comparable, mapping=aes(x=collectDate, y=externalConductance), col="red2", shape=3)+
  xlim(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-01"))+ #must convert date ranges to POSIXct to match input data
  ylim(0,30)


## Average values from domain samples

domain_mean_conductance=
  swc_domain_comparable %>%
  group_by(collectDate) %>%
  summarize(avg_domain_conductance=mean())

# Merge averaged domainLab values with externalLab values by collection date
combined_conductance=merge(domain_mean_conductance, swc_external_comparable, by="collectDate")

# plot a comparison of conductance values between domainLab and externalLab with 
ggplot(data=combined_conductance, mapping=aes(x=avg_domain_conductance, y=externalConductance))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Averaged domain lab conductivity")+
  ylab("External lab conductivity")

# read the summary information for the linear model here
summary(lm(data=combined_conductance, externalConductance ~ avg_domain_conductance))

cor.test(combined_pH$avg_domain_pH,combined_pH$pH)

#Woah, look at that outlier! What if we remove it..

combined_conductance_corrected=combined_conductance[combined_conductance$externalConductance<100,]

# plot a comparison of conductance values between domainLab and externalLab with 
ggplot(data=combined_conductance_corrected, mapping=aes(x=avg_domain_conductance, y=externalConductance))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Averaged domain lab conductivity")+
  ylab("External lab conductivity")

# read the summary information for the linear model here
summary(lm(data=combined_conductance_corrected, externalConductance ~ avg_domain_conductance))

cor.test(combined_conductance_corrected$externalConductance, combined_conductance_corrected$avg_domain_conductance)


## Compare ALC.ALK instead

# plot a comparison of conductance values between domainLab and externalLab with 
ggplot(data=combined_conductance, mapping=aes(x=avg_domain_conductance, y=externalConductance))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Averaged domain lab conductivity")+
  ylab("External lab conductivity")

###
##### Section 2 - compare EPT taxa between KING and MCDI
###
rm(list=ls())

EPT_full=loadByProduct(dpID = "DP1.20120.001", 
                       site = c("KING","MCDI"), package = "expanded",check.size = T)

# use list2env() again to convert EPT_full object to more useful data.frames
list2env(EPT_full, .GlobalEnv)

# First, there is an issue that individuals sampled are identified to different taxonomic levels
# so we use the dplyr package's "pipe" notation to count the number of individuals identified
# at each taxonomic level
taxonomic_specificity =
  inv_taxonomyProcessed %>%
  group_by(siteID, taxonRank) %>%
  summarize(count=n())

ggplot(data=taxonomic_specificity, aes(x=fct_reorder(taxonRank, count, .desc=T), # Use fct_reorder() to arrange in descending order
                                       y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic Level Identified")


# Note, to get the bars to plot in taxonomic order you could change the
# order of the factor levels as shown here:
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/


## Suppose we only want to look at the diversity at the order level (e.g., EPT taxa). 
#  We can count the number of observations for each famil at each site, 
#  again using the dplyr functions and 'pipes':

orders =
  inv_taxonomyProcessed %>%
  group_by(siteID, order) %>%
  summarize(count=n())

# Let's plot these observations grouped by family, and filled in by siteID

ggplot(data=orders, 
       aes(x=fct_reorder(order, count, .desc=T), #use fct_reorder to organize by number of observations
           y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic family name")

## Looks like there are lots of diptera (midges?)
#  After talking with Joe, we decided to remove those from the dataset and calculate the
#  percentage that EPT make up of the population:

# remove Diptera

orders=orders[orders$order!="Diptera",]
orders = orders[complete.cases(orders),] # this line gets rid of the "NAs" that result from the line above

# So let's plot these again
ggplot(data=orders, 
       aes(x=fct_reorder(order, count, .desc=T), #use fct_reorder to organize by number of observations
           y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic family name")

# Wow, so ephemeroptera and tricoptera are in the top three orders represented, that's great!

EPT_pct_df =
  orders %>%
  group_by(siteID) %>% 
  mutate(Tax_pct = (count/sum(count))) %>% 
  filter(order %in% c("Ephemeroptera","Trichoptera","Plecoptera")) %>% 
  group_by(siteID) %>% 
  summarise(EPT_pct=sum(Tax_pct))

# Display EPT as a percentage of each site's catch
EPT_pct_df

## Now, can we calculate EPT% for ALL sites?

rm(list=ls())

EPT_full=loadByProduct(dpID = "DP1.20120.001", 
                       site = "all", package = "expanded",check.size = T)

# use list2env() again to convert EPT_full object to more useful data.frames
list2env(EPT_full, .GlobalEnv)

orders =
  inv_taxonomyProcessed %>%
  group_by(siteID, order) %>%
  summarize(count=n())

## including Diptera
EPT_pct_df =
  orders %>%
  group_by(siteID) %>% 
  mutate(Tax_pct = (count/sum(count))) %>% 
  filter(order %in% c("Ephemeroptera","Trichoptera","Plecoptera")) %>% 
  group_by(siteID) %>% 
  summarise(EPT_pct=sum(Tax_pct))
EPT_pct_df

# remove Diptera

orders_no_midges=orders[orders$order!="Diptera",]
orders_no_midges = orders_no_midges[complete.cases(orders_no_midges),] # this line gets rid of the "NAs" that result from the line above

EPT_pct_df_no_midges =
  orders_no_midges %>%
  group_by(siteID) %>% 
  mutate(Tax_pct = (count/sum(count))) %>% 
  filter(order %in% c("Ephemeroptera","Trichoptera","Plecoptera")) %>% 
  group_by(siteID) %>% 
  summarise(EPT_pct=sum(Tax_pct))
EPT_pct_df_no_midges

write.csv(EPT_pct_df, file = "EPT_pct_including_diptera.csv", row.names = F)
write.csv(EPT_pct_df_no_midges, file = "EPT_pct_excluding_diptera.csv", row.names = F)
