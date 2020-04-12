#Section - 1(a)

my_dataframe <- read.csv("C:/Users/Saketh Vanam/Downloads/CA2/NIPostcodes.csv", header = FALSE)
nrow(my_dataframe)
ncol(my_dataframe)
str(my_dataframe)
head(my_dataframe, n=10)

#Section - 1(b)

new_colnames <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                  "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                  "X-Cordinates","Y-Cordinates","Primary_Key")

colnames(my_dataframe) <- new_colnames
colnames(my_dataframe)

#Section -1(c)

my_dataframe[my_dataframe==""] <- NA
sum(is.na(my_dataframe))
sum(!complete.cases(my_dataframe))


install.packages("mice")
install.packages("VIM")
library(mice)
md.pattern(my_dataframe)
library("VIM")
missing_values <- aggr(my_dataframe, prop = FALSE, numbers = TRUE)

#section - 1(d)

Missing_Count <- sapply(my_dataframe, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)
Missing_Count

#section -1(e)

my_dataframe <- my_dataframe[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
my_dataframe

#section - 1(f)
Limavady_data <- my_dataframe[which(my_dataframe$Locality == "LIMAVADY" | my_dataframe$Townland == "LIMAVADY" & my_dataframe$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)


#section - 1(g)

write.csv(Limavady_data,"Limavady.csv")
write.csv(my_dataframe,"CleanNIPostcodeData.csv")
