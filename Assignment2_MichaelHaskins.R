## Levels function is very useful
 ## Note: levels function will not work if using it on a character vector. levels( charVector ) will result in NULL, if dealing with a char vector use unique( charVector ) instead
## Should prob use write.csv

############################ PART 1 ##############################

EUData = read.csv(file = "Lab 2 - EU_20190102.txt", header = TRUE)
?read.table
USData = read.csv(file = "Lab 2 - US_2019_02_01.txt", header = TRUE, sep = ";")

## subsetting data :
EU_cust_30_50 = subset( EUData, (age >= 30) & (age <= 50))
write.table(EU_cust_30_50, file = "EU_cust_30_50.csv", row.names = FALSE, sep = ",")

EU_cust_ter_se_enter = subset( EUData, (education == "tertiary") & ((job == "self-employed") | (job == "entrepreneur")))
write.table(EU_cust_ter_se_enter, file = "EU_cust_ter_se_enter.csv", row.names = FALSE, sep = ",") ##Files stored in WD folder. Note she used write.csv

## Check if worked correctly by:
table( EU_cust_ter_se_enter$job, EU_cust_ter_se_enter$education ) ## Can see it has worked correctly
?write.table


############################## PART 2 ################################
## Dividing the dataset into occupationsa and then saving these as individual datasets called <occupation>.csv:
?levels
levels( EUData[ ,2])
for( occupation in levels( EUData[ ,2])){  ## occupation will be admin. on first iteration then blue-collar and so on
 # print(occupation)
  DataFrame = subset(EUData, job == occupation)
  if( occupation == "admin."){
     ?substr
     # substr("admin.", 0, 5) # Results in admin
     write.table(DataFrame, file = paste(  substr("admin.", 0, 5) , ".csv", sep = ""))
   } else {
     write.table(DataFrame, file = paste(occupation, ".csv", sep = ""))
   }
}

############################# PART 3 ################################
## Making 2 datasets ready to merge:

# [Assume the Euro/Dollar conversion rate is 1 US Dollar = 0.87  Euro]
EUAmmended = EUData
USAmmended = USData

dim(EUData)
dim(USData)

levels(EUAmmended[,9])[1] <- "teleph"  ## Changes all cellular entries to teleph 
levels(EUAmmended[,9])
levels(EUAmmended[,9])[2] <- "teleph"  ## Changes all telephone entries to telph

?data.frame
?sapply

## Changing the US Dollars Balance to Euro
USAmmended$balance <- USAmmended$balance * 0.87 ## This updates the balance field
##THis didnt work, only created a vector: sapply(USData$balance, '*', 0.87)

## Removing "poutcome" field from EUAmmended data frame
EUAmmended$poutcome <- NULL ## This sets the field to null which removes it
dim(EUAmmended)

## Add Region field with values EU and US
USAmmended$region <- "US"
EUAmmended$region <- "EU"

##Now combine the two dataframes using rbind
USEUData = rbind(USAmmended, EUAmmended)

## Now write new data frame to csv file
write.table(USEUData, file = "EU_US_Merge.csv", row.names = FALSE, sep = ",")
