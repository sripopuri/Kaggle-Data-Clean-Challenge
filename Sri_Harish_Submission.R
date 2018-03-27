# Clearing my workspace
rm(list = ls())

# Setting the working directory
setwd("D:/New folder/Data Cleaning Challenge/Day 1/Building_Permits.csv")

# Getting the data
data = read.csv(file = 'Building_Permits.csv',na.strings = "")

dim(data)
# 198900 rows and 43 columns
str(data)
summary(data)
# Can see missing values in some columns

head(data,n = 3)
# Missing values are visible in the first few rows!

# Lets get the count of missing values in the data
sum(is.na(data))
# 2245941 cells of data have NA's

# Getting the NA's count columns wise
result = apply(data,2,function(x){sum(is.na(x))})
result[result > 0]
# Are the columns which had NAs and these are 12 in number
# Hence 31 columns dont have NAs and 12 columns have NAs
length(result[result > 0])

# Let us sort them inorder to see 
# Which columns have the least number and highest
# number of NAs
sort(result[result > 0])

missing = sum(result) # <- Missing value cells with NAs
total = dim(data)[1]*dim(data)[2]

# Hence percentage of values/cells that are 
# missing in the data are 
percent_miss = missing/total
percent_miss
# So nearly 26% of the cells are having missing values 

# In Street number Suffix there are no NAs
# On th other hand, there are 1716 NAs in the Zipcode

write.csv(x = names(result[result > 0]),file = "Columns.csv")

# Since location is not mentioned in these records,
# the Zipcode and Supervisor district is also NA
# These are 1700 rows in number
nrow(data[is.na(data$Zipcode) 
          & is.na(data$Supervisor.District)
          & is.na(data$Location),])


nrow(data[is.na(data$Zipcode) 
          & is.na(data$Supervisor.District)
          & is.na(data$Location)
          & is.na(data$Neighborhoods...Analysis.Boundaries),])


######################## Completed Date
class(data$Completed.Date)
sum(is.na(data$Completed.Date))

nrow(data[data$Current.Status != 'complete',])
# 101823 cases' are not in completed status
# Hence these can have an NA value in the Completed date
# But we have only 101709 cases as NA in Completed date

# This below is expected to have 0 rows
nrow(data[data$Current.Status == 'complete'
          & is.na(data$Completed.Date),])

# These are 101709 rows in number
nrow(data[data$Current.Status != 'complete'
          & is.na(data$Completed.Date),])

# Hence for these instances the completed date is populated 
# though the current status is not 'complete'
unique(data[!is.na(data$Completed.Date)
            & data$Current.Status != 'complete','Current.Status'])

unique(data[!is.na(data$Completed.Date)
            & data$Current.Status != 'complete','Completed.Date'])
# These are some valid dates 
# I believe these dates should not be populated.
# Populating a completed date for a cancelled status doesnt make any sense!

nrow(data[!is.na(data$Completed.Date)
          & data$Current.Status != 'complete',])

colnames(data)

dates = data[!is.na(data$Completed.Date)
             & data$Current.Status != 'complete',c(14:18,25)]

write.csv(dates,file = "dates.csv")

nrow(dates[as.Date(dates$Current.Status.Date) >= as.Date(dates$Completed.Date),])
rest = dates[as.Date(dates$Current.Status.Date) < as.Date(dates$Completed.Date),]


######################## TIDF Compliance
# There are only two rows with the TIDF commpliance value
# Hence remove this column
sum(!is.na(data$TIDF.Compliance))
unique(data$TIDF.Compliance)

######################## Voluntary soft story retrofit
# There are only 35 rows with the Voluntary soft story retrofit value
# The rest of the rows are assumed to not meet the soft story
# earth regulations
sum(!is.na(data$Voluntary.Soft.Story.Retrofit))
unique(data$Voluntary.Soft.Story.Retrofit)
table(data$Voluntary.Soft.Story.Retrofit)

######################## Fire only permit
# There are only 18827 rows which have the fire only permit
# Hence assuming that there is no permit for other properties
sum(!is.na(data$Fire.Only.Permit))
table(data$Fire.Only.Permit)
sort(result[result > 0])

######################## Description
# Leaving this column as it is since the column is a text field
# and might have been the case that there was no description entered
# by the Data handler

######################## Revised cost
class(data$Revised.Cost)
summary(data$Revised.Cost)

nrow(data[data$Revised.Cost == 0,])
nrow(data[data$Revised.Cost == 1,])
nrow(data[is.na(data$Revised.Cost),])
sort(result[result > 0])
colnames(data)

######################## Street Number Suffix
# There are 196684 Nas in the column
# and hence this column can be ignored

######################## Site permit
summary(data$Site.Permit)
class(data$Site.Permit)
# There is permit for site for 5359 instances
# the other instances can be assumed to not have a permit


###########################
# Proposed.Construction.Type & Proposed.Construction.Type.Description
# There are 43162 NAs in the column Proposed.Construction.Type 
# hence the same NAs in the Proposed.Construction.Type.Description
summary(data$Proposed.Construction.Type)

nrow(data[is.na(data$Proposed.Construction.Type) 
          & is.na(data$Proposed.Construction.Type.Description),])
table(data$Proposed.Construction.Type,data$Proposed.Construction.Type.Description)


table(data$Existing.Construction.Type,data$Existing.Construction.Type.Description)
nrow(data[is.na(data$Existing.Construction.Type) 
          & is.na(data$Existing.Construction.Type.Description),])

# So CHecking
nrow(data[is.na(data$Proposed.Construction.Type) 
          & is.na(data$Existing.Construction.Type),])


# Summary suggests that this field needs to be factorized
class(data$Proposed.Construction.Type)
class(data$Proposed.Construction.Type.Description)

unique(data$Permit.Type.Definition)
table(data$Permit.Type.Definition)
table(data$Permit.Type.Definition,data$Existing.Construction.Type.Description)
table(data[is.na(data$Existing.Construction.Type),'Permit.Type.Definition'])

table(data$Existing.Construction.Type,data$Proposed.Construction.Type)