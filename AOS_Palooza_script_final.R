## AOS Palooza data exploration script
## By Donal O'Leary of Battelle's National Ecological Observatory Network (NEON)
## For the 2020 AOS Palooza
## These materials are based on the NEONscience.org data skills tutorials
## and referenced where appropiriate.
## Full credit to the respective authors of those tuorials. 

# install.packages("neonUtilities")
# install.packages("tidyverse")
# install.packages("reshape2")

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

# We can also see that there are three separate tables for the domainLabData 
# and the externalLabData, and they measure different things!
names(swc_domainLabData)
names(swc_externalLabData)

## There are also two separate files for the data that are collected in the field:
names(swc_fieldData)
names(swc_fieldSuperParent) # this 'SuperParent' table contains all of the measured values

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
# the melt() function reshapes a data.frame into a format that is better for ggplot
swc_fieldMelt=melt(swc_fieldSuperParent, id.vars=c("parentSampleID","collectDate","location"), measure.vars=c("dissolvedOxygen","specificConductance"))

ggplot(data=swc_fieldMelt[swc_fieldMelt$location %in% c("C1","C0","C2"),], mapping=aes(x=collectDate))+ # removed IN and OT because they make 2015/2016 look very messy
  geom_line(data=swc_fieldSuperParent[swc_fieldSuperParent$location %in% c("C1","C0"),], mapping=aes(y=waterTemp),col="blue")+
  geom_point(mapping=aes(y=value, shape=location, color=variable))+
  geom_point(mapping=aes(y=value, shape=location, color=variable))+
  ylab("DO (pct%), Conductance (ms/ml), and Temp (*C)")+
  ggtitle("Hand-collected Field Data")


## these lines combine the different tables (field, domainLab, externalLab) into a single table by the sampleID
swc_combined=full_join(x=swc_domainLabData, y=swc_externalLabData, by=c("parentSampleID"="sampleID"))
swc_combined=full_join(x=swc_combined, y=swc_fieldSuperParent, by="parentSampleID")

### Compare external Lab bicarbonate and Domain lab ALK
## Question from Donal - is this fair to compare these values?
# plot a comparison of ALK values between domainLab and externalLab with 
ggplot(data=swc_combined, mapping=aes(x=alkMgPerL, y=waterBicarbonate, shape=location.x, color=location.x))+ #define x and y axes here
  geom_point()+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  #stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain ALK")+
  ylab("External lab bicarbonate")

## Wow! That's a big outlier. Let's zoom in on the more relevant data points
ggplot(data=swc_combined, mapping=aes(x=alkMgPerL, y=waterBicarbonate, shape=location.x, color=location.x))+ #define x and y axes here
  geom_point()+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  #stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain ALK")+
  ylab("External lab bicarbonate")+
  xlim(-3,10)+
  ylim(0,15)

##Interesting to see which locations appear to have bias compared to the 1:1 dashed line
# for example the squares (C2) have many observations where the domain ALK is far higher than the external lab bicarbonate
# where as the triangles (C1) show the opposite bias
# How does this compare to the inherent measurement uncertainty for each (titration?) method?


### Compare domain vs external lab conductivity
## Conductivity should be relatively straightforward to measure and compare
## unless, of course, changes in temperature and chemistry have occurred in transit to the external lab
# plot a comparison of conductivity between external lab and field observation
ggplot(data=swc_combined, mapping=aes(x=specificConductance, y=externalConductance, color=location.x, shape=location.x))+ #define x and y axes here
  geom_point()+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  #stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain field conductivity")+
  ylab("External conductivity")

## Again! A single outlier really throws things off. Let's focus the plot on the relevant values

ggplot(data=swc_combined, mapping=aes(x=specificConductance, y=externalConductance, color=location.x, shape=location.x))+ #define x and y axes here
  geom_point()+ #plot the data points
  geom_abline(slope=1, intercept=0, lty=2, lwd=1)+ # add a dashed 1:1 line in black
  #stat_smooth(method = "lm", col = "red2")+ #add linear model with confidence interval in red/grey
  xlab("Domain field conductivity")+
  ylab("External conductivity")+
  xlim(0,30)+
  ylim(0,30)



###
##### Section 2 - compare EPT taxa between KING and MCDI
###
rm(list=ls())

EPT_full=loadByProduct(dpID = "DP1.20120.001", 
                       site = c("KING","MCDI"), package = "expanded",check.size = F)

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
                       site = "all", package = "expanded",check.size = F)

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
