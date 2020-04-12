#Section -2(a)

rm(list = ls(all=TRUE))
getwd()
setwd("C:/Users/Saketh Vanam/Downloads/CA2/NI Crime Data/NI Crime Data")
File=list.files(pattern = "[.]csv$",recursive = T)
AllNICrimeData_List=lapply(File,function(x)read.csv(x, header = TRUE))
AllNICrimeData=do.call("rbind",AllNICrimeData_List)
write.csv(AllNICrimeData,"AllNICrimeData.csv")
nrow(AllNICrimeData)

#section - 2(b)

AllNICrimeData_new = subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category) )
AllNICrimeData_new

#section - 2(c)

unique(AllNICrimeData$Crime.type)
library(dplyr)

AllNICrimeData_new=
  AllNICrimeData_new %>% mutate(Crime.type=recode_factor(Crime.type, 
                                                         'Anti-social behaviour' = 'ASBO', 
                                                         'Bicycle theft' = 'BITH',
                                                         'Burglary' = 'BURG',
                                                         'Criminal damage and arson' = 'CDAR',
                                                         'Drugs' = 'DRUG', 
                                                         'Other theft' = 'OTTH',
                                                         'Possession of weapons' = 'POFW',
                                                         'Public order' = 'PUBO',
                                                         'Robbery' = 'ROBY', 
                                                         'Shoplifting' = 'SHOP',
                                                         'Theft from the person' = 'THPR',
                                                         'Vehicle crime' = 'VECR',
                                                         'Violence and sexual offences'='VECO',
                                                         'Other crime' = 'OTCR'))
AllNICrimeData_new

#section - 2(d)

counts <- table(AllNICrimeData_new$Crime.type)
my_plot = barplot(counts,main="Distribution of Crime Type"
                  ,ylab = 'freq',
                  col=c( rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.9,0.9,0.4,0.6) ,  rgb(0.3,0.3,0.4,0.6),rgb(0.7,0.1,0.4,0.6) , rgb(0.1,0.5,0.4,0.6) , rgb(0.3,0.9,0.9,0.6) ,  rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.1,0.9,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.9) ,  rgb(0.3,0.9,0.4,0.9)),
                  xlab = 'Crime Type',las=2)
text(my_plot, counts/2 , paste("", counts, sep="") ,cex=1)


#section -2(e)

AllNICrimeData_new$Location <- gsub('On or near ', '', AllNICrimeData_new$Location)
AllNICrimeData_new


#section -2(f)

AllNICrimeData_new=AllNICrimeData_new[!( AllNICrimeData_new$Location==""), ]

rows=seq(1,nrow(AllNICrimeData_new),1)
set.seed(100)

random_crime_sample=AllNICrimeData_new[sample(rows, 5000), ]

CleanNIPostcodeData = read.csv('CleanNIPostcodeData.csv',header = TRUE)

head(CleanNIPostcodeData)

find_a_town <- function(i) {
  random_crime_sample$City_Town_Village<-CleanNIPostcodeData[match(toupper(random_crime_sample$Location),CleanNIPostcodeData$Primary.Thorfare),11]
}

find_a_town(toupper(random_crime_sample$Location))
