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

# download data product and load into environment
# for an excellent tutorial see: https://www.neonscience.org/download-explore-neon-data
h2o_chemistry_full=loadByProduct(dpID="DP1.20093.001", site="FLNT", package = 'expanded',check.size = T)
#loadByProduct() gives us a big, complex 'list', that we called h2o_chemistry_full

# To view the use guide for this product, go to:
# https://data.neonscience.org/documents/10179/2237401/NEON_waterChem_userGuide_vA/4974e1a3-1937-470c-ac29-061a54524bf0

# To make this easier to work with, we will use the list2env() function to give us the individual 
# list items as data.frames
list2env(h2o_chemistry_full, .GlobalEnv)

# Let's take a look at all of the variables available to us in this data product:
View(variables)

###
# Use swc_fieldSuperParent instead! (these are the field handheld readings)

# We can also see that there are two separate tables for the domainLabData and the externalLabData
names(swc_domainLabData)
names(swc_externalLabData)

unique(swc_domainLabData$collectDate)
unique(swc_externalLabData$collectDate)

# Convert all times to POSIXct
swc_domainLabData$collectDate=as.POSIXct(swc_domainLabData$collectDate)
swc_externalLabData$collectDate=as.POSIXct(swc_externalLabData$collectDate)
swc_fieldSuperParent$collectDate=as.POSIXct(swc_fieldSuperParent$collectDate)

## extract only samples from same date
## because the latest samples haven't returned from the eternal lab yet

# select only the samples from the domainLab with dates that match the externalLab's dates
swc_domain_comparable=swc_domainLabData[swc_domainLabData$parentSampleID %in% swc_externalLabData$sampleID,]

# likewise, select only the external lab samples with daets that match 
swc_external_comparable=swc_externalLabData[swc_externalLabData$sampleID %in% swc_domainLabData$parentSampleID,]

ggplot()+
  geom_point(swc_domain_comparable, mapping=aes(x=collectDate, y=initialSamplepH))+
  geom_point(data=swc_external_comparable, mapping=aes(x=collectDate, y=pH), col="red2")+
  xlim(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-01")) #must convert date ranges to POSIXct to match input data


## Average values from domain samples

domain_mean_pH=
  swc_domain_comparable %>%
  group_by(collectDate) %>%
  summarize(avg_domain_pH=mean(initialSamplepH))

# Merge averaged domainLab values with externalLab values by collection date
combined_pH=merge(domain_mean_pH, swc_external_comparable, by="collectDate")

# plot a comparison of pH values between domainLab and externalLab with 
ggplot(data=combined_pH, mapping=aes(x=avg_domain_pH, y=pH))+ #define x and y axes here
  geom_point(col="blue2")+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Averaged domain lab pH")+
  ylab("External lab pH")

# read the summary information for the linear model here
summary(lm(data=combined_pH, pH ~ avg_domain_pH))

cor.test(combined_pH$avg_domain_pH,combined_pH$pH)


#######
# Compare conductivity

# select only the samples from the domainLab with dates that match the externalLab's dates
swc_domain_comparable=swc_fieldSuperParent[swc_fieldSuperParent$parentSampleID %in% swc_externalLabData$sampleID,]

# likewise, select only the external lab samples with daets that match 
swc_external_comparable=swc_externalLabData[swc_externalLabData$sampleID %in% swc_fieldSuperParent$parentSampleID,]

ggplot()+
  geom_point(swc_domain_comparable, mapping=aes(x=collectDate, y=specificConductance))+
  geom_point(data=swc_external_comparable, mapping=aes(x=collectDate, y=externalConductance), col="red2")+
  xlim(as.POSIXct("2017-01-01"),as.POSIXct("2017-12-01")) #must convert date ranges to POSIXct to match input data


## Average values from domain samples

domain_mean_conductance=
  swc_domain_comparable %>%
  group_by(collectDate) %>%
  summarize(avg_domain_conductance=mean(specificConductance))

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

combined_conductance_corrected=combined_conductance[combined_conductance$externalConductance<500,]

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


##### Section 2 - compare EPT taxa between KING and MCDI

rm(list=ls())

EPT_full=loadByProduct(dpID = "DP1.20120.001", site = c("KING","MCDI"), package = "expanded",check.size = T)

# use list2env() again to convert EPT_full object to more useful data.frames
list2env(EPT_full, .GlobalEnv)

# First, there is an issue that individuals sampled are identified to different taxonomic levels
# so we use the dplyr package's "pipe" notation to count the number of individuals identified
# at each taxonomic level
taxonomic_specificity =
  inv_taxonomyProcessed %>%
  group_by(siteID, taxonRank) %>%
  summarize(count=n())

## three different ways to plot it!
barplot(taxonomic_specificity$count, 
        names.arg = paste(taxonomic_specificity$siteID, taxonomic_specificity$taxonRank, sep=" "),
        las=2)

ggplot(data=taxonomic_specificity, aes(x=taxonRank, y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=taxonomic_specificity, aes(x=taxonRank, y=count, fill=siteID))+
  geom_bar(stat="identity")+
  facet_wrap(~siteID)+
  theme(axis.text.x = element_text(angle = 90))

# Note, to get the bars to plot in taxonomic order you could change the
# order of the factor levels as shown here:
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/


## Suppose we only want to look at the diversity at the family level. We can count the number of 
#  observations for each famil at each site, again using the dplyr functions and 'pipes':

families =
  inv_taxonomyProcessed %>%
  group_by(siteID, family) %>%
  summarize(count=n())

# Let's plot these observations grouped by family, and filled in by siteID

ggplot(data=families, aes(x=family, y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic family name")

# Welp, that's messy! Let's re-order the factor levels from largest to smallest

ggplot(data=families, 
       aes(x=fct_reorder(family, count, .desc=T), #use fct_reorder to organize by number of observations
           y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic family name")

# Better, but that plot is kind of hard to see because there are many families with only a few observations.
# So, we can plot only those families with >3 observations to focus on the 'major' taxa

ggplot(data=families[families$count>3,], # remove families with very low number of observations
       aes(x=fct_reorder(family, count, .desc=T), #use fct_reorder to organize by number of observations
           y=count, fill=siteID))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Taxonomic family name")

       