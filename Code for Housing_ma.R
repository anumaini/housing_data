#load the data 
housing_data <- read.csv(file.choose(), header=T)

#Packages used
library(dplyr)

#converting blanks to NA
Variables <- variable.names(housing_data) # converting all variable names to  
housing_data <- data.frame(ifelse(housing_data %in% c(""," ","NA"), NA, housing_data))#Treating blanks and reconverting list to Data Frame 
colnames(housing_data) <- Variables # renaming the strings 

# Data Assesment  
names(housing_data)
summary(housing_data)
str(housing_data)

#converting class based variables to one collumn 




class(housing_data[1,])






#understanding the variables' measurement type 

Variable_names <- data.frame(sapply(housing_data, class))
  
  
