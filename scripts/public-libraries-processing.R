library(plyr)
library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)

##################################################################
#
# Processing Script for Public Libraries
# Created by Jenna Daly
# On 05/11/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
hist_lib_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "CTPublicLibraries") 
income_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "CTLibStatProfile-") 
current_lib_xlsx <-  dir(path_to_raw_data, recursive=T, pattern = "Addendum") 
hist_lib_df <- (read_excel(paste0(path_to_raw_data, "/", hist_lib_xlsx), sheet=1, skip=0, col_types = "text"))
income_df <- (read_excel(paste0(path_to_raw_data, "/", income_xlsx), sheet=13, skip=0, col_types = "text"))

#read in all sheets from current years summary table
#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

#Create a list of all sheet data for all years
#for (i in 1:length(current_lib_xlsx)) {
  mysheets <- read_excel_allsheets(paste0(path_to_raw_data, "/", current_lib_xlsx[1]))
  #Cycle through all sheets, extract sheet name, and assign each sheet (county) its own data frame
  for (j in 1:length(mysheets)) {
    current_sheet_name <- names(mysheets)[j]
    current_sheet_file <- mysheets[[j]] 
    assign(current_sheet_name, current_sheet_file)
  }
#}

#Start to build columns for final df
library_stats <- data.frame(stringsAsFactors = F)
library_stats <- UseStatsA
colnames(library_stats) <- library_stats[2,]  
library_stats <- library_stats[-c(1:7),]
library_stats <- library_stats %>% 
  select(1,2,3,5,7,9,10,12) %>% 
  rename(Town = `SELECTED LIBRARY USE STATISTICS BY TOWN/CITY - Towns with multiple reporting libraries are in gray`)

library_services <- data.frame(stringsAsFactors = F)
library_services <- ServicesA
colnames(library_services) <- library_services[2,]
library_services <- library_services[-c(1:7),]
library_services <- library_services %>% 
  select(1,14,17) %>% 
  rename(Town = `SERVICES BY TOWN/CITY - Towns with multiple reporting libraries are in gray`)

library_collection <- data.frame(stringsAsFactors = F)
library_collection <- CollectionA
colnames(library_collection) <- library_collection[2,]
library_collection <- library_collection[-c(1:7),]
#remove duplicate column names
library_collection <- library_collection %>% 
  select(1,21) %>% 
  rename(Town = `COLLECTIONS BY TOWN/CITY - Towns with multiple libraries are in gray`)

library_income <- data.frame(stringsAsFactors = F)
library_income <- IncomeA
colnames(library_income) <- library_income[2,]
library_income <- library_income[-c(1:7),]
library_income <- library_income %>% 
  select(1,4,9,27) %>% 
  rename(Town = `INCOME BY TOWN/CITY - Towns with multiple reporting libraries are in gray`)

library_expenditures <- data.frame(stringsAsFactors = F)
library_expenditures <- ExpenditureA
colnames(library_expenditures) <- library_expenditures[2,]
library_expenditures <- library_expenditures[-c(1:7),]
library_expenditures <- library_expenditures %>% 
  subset(., select=which(!duplicated(names(.)))) %>%  #only selects from those columns that are not duplicates
  select(1,8,12,26) 
names(library_expenditures)[names(library_expenditures) == "EXPENDITURES BY TOWN/CITY - Towns with multiple reporting libraries are in gray"] <- "Town"

total_library <- join_all(list(library_stats, library_collection, library_income, library_expenditures, library_services), by = 'Town', type = 'full')
total_library$Year <- 2016

#Calculated columns
#set variables to numeric
total_library$"Population of Service Area 2015" <- as.numeric(total_library$"Population of Service Area 2015")
total_library$"Total Circulation" <- as.numeric(total_library$"Total Circulation")
total_library$"Total Physical Collection" <- as.numeric(total_library$"Total Physical Collection")
total_library$"Internet Computers For The Public" <- as.numeric(total_library$"Internet Computers For The Public")
total_library$"Expenditure on Library Materials All Types" <- as.numeric(total_library$"Expenditure on Library Materials All Types")
total_library$"Total Library Visits" <- as.numeric(total_library$"Total Library Visits")
total_library$"Library's Municipal Appropriation 2015/2016" <- as.numeric(total_library$"Library's Municipal Appropriation 2015/2016")
total_library$"Total Operating Expenditures" <- as.numeric(total_library$"Total Operating Expenditures")
total_library$"Total Operating Income" <- as.numeric(total_library$"Total Operating Income")
total_library$"Total Program Attendance" <- as.numeric(total_library$"Total Program Attendance")
total_library$"Reference Transactions" <- as.numeric(total_library$"Reference Transactions")
total_library$"Total Registered Resident Borrowers (Principal Library)" <- as.numeric(total_library$"Total Registered Resident Borrowers (Principal Library)")
total_library$"Income From State Funds" <- as.numeric(total_library$"Income From State Funds")
total_library$"Wages & Salaries" <- as.numeric(total_library$"Wages & Salaries")


total_library$"Circulation per capita" <- NA
total_library$"Circulation per capita" <- total_library$"Total Circulation" / total_library$"Population of Service Area 2015"

total_library$"Collection Items per capita" <- NA      
total_library$"Collection Items per capita" <- total_library$"Total Physical Collection" / total_library$"Population of Service Area 2015"

total_library$"Internet Use per capita" <- NA          
total_library$"Internet Use per capita" <- total_library$"Internet Computers For The Public" / total_library$"Population of Service Area 2015"

total_library$"Library Materials Expenditure per capita" <- NA    
total_library$"Library Materials Expenditure per capita" <- total_library$"Expenditure on Library Materials All Types" / total_library$"Population of Service Area 2015"

total_library$"Library Visits per capita" <- NA                   
total_library$"Library Visits per capita" <- total_library$"Total Library Visits" / total_library$"Population of Service Area 2015"

total_library$"Municipal Appropriation per capita" <- NA          
total_library$"Municipal Appropriation per capita" <- total_library$"Library's Municipal Appropriation 2015/2016" / total_library$"Population of Service Area 2015"

total_library$"Operating Expenditures per capita" <- NA           
total_library$"Operating Expenditures per capita" <- total_library$"Total Operating Expenditures" / total_library$"Population of Service Area 2015"

total_library$"Operating Income per capita" <- NA                 
total_library$"Operating Income per capita" <- total_library$"Total Operating Income" / total_library$"Population of Service Area 2015"

total_library$"Program Attendance per capita" <- NA               
total_library$"Program Attendance per capita" <- total_library$"Total Program Attendance" / total_library$"Population of Service Area 2015"

total_library$"Reference Questions per capita" <- NA              
total_library$"Reference Questions per capita" <- total_library$"Reference Transactions" / total_library$"Population of Service Area 2015"

total_library$"Registered Borrowers per capita" <- NA             
total_library$"Registered Borrowers per capita" <- total_library$"Total Registered Resident Borrowers (Principal Library)" / total_library$"Population of Service Area 2015"

total_library$"State Appropriation per capita" <- NA              
total_library$"State Appropriation per capita" <- total_library$"Income From State Funds" / total_library$"Population of Service Area 2015"

total_library$"Wages and Salaries Expenditures per capita" <- NA  
total_library$"Wages and Salaries Expenditures per capita" <- total_library$"Wages & Salaries" / total_library$"Population of Service Area 2015"

  

  
  
  
  
  






current_lib_df <- (read_excel(paste0(path_to_raw_data, "/", current_lib_xlsx), sheet=13, skip=0, col_types = "text"))


lib_town_xwalk <- read.csv(paste0(path_to_raw_data, "/", "library_town_crosswalk.csv"), stringsAsFactors = F, header = T)

#Isolate columns needed from lib_df for data set
lib_df <- lib_df[,c("Selected Library StatisticsUse the Filter Tool to Choose Your Library", 
                     "Fiscal Year", 
                     "AENGLC Wealth Rank", 
                     "Population of Service Area", 
                     "Total Library Visits", 
                     "Total Registered Borrowers", 
                     "Reference Questions", 
                     "Total Circulation", 
                     "Total Programs", 
                     "Total Program Attendance",
                     "Use of Public Internet Computers",
                     "Total Collection Size",
                     "Total Operating Income",
                     "Municipal Appropriation",
                     "Library Materials Expenditures",
                     "Wages & Salaries Expenditures",
                     "Operating Expenditures" 
                     )]

#remove first row from lib_df
lib_df <- lib_df[-1,]

#rename town names
names(lib_df)[names(lib_df) == "Selected Library StatisticsUse the Filter Tool to Choose Your Library"] <- "Town.Library"
lib_df_merge <- merge(lib_df, lib_town_xwalk, by = "Town.Library", all=T)

lib_df_merge[lib_df_merge == "N/A"] <- NA

lib_df_agg <- data.frame(lapply(lib_df_merge, as.character), stringsAsFactors=FALSE)

lib_df_agg$"Fiscal.Year" <- as.character(lib_df_agg$"Fiscal.Year") 
lib_df_agg$"AENGLC.Wealth.Rank" <- as.numeric(lib_df_agg$"AENGLC.Wealth.Rank") 
lib_df_agg$"Population.of.Service.Area" <- as.numeric(lib_df_agg$"Population.of.Service.Area")
lib_df_agg$"Total.Library.Visits" <- as.numeric(lib_df_agg$"Total.Library.Visits")
lib_df_agg$"Total.Registered.Borrowers" <- as.numeric(lib_df_agg$"Total.Registered.Borrowers")
lib_df_agg$"Reference.Questions" <- as.numeric(lib_df_agg$"Reference.Questions")
lib_df_agg$"Total.Circulation" <- as.numeric(lib_df_agg$"Total.Circulation")
lib_df_agg$"Total.Programs" <- as.numeric(lib_df_agg$"Total.Programs")
lib_df_agg$"Total.Program.Attendance" <- as.numeric(lib_df_agg$"Total.Program.Attendance")
lib_df_agg$"Use.of.Public.Internet.Computers" <- as.numeric(lib_df_agg$"Use.of.Public.Internet.Computers")
lib_df_agg$"Total.Collection.Size" <- as.numeric(lib_df_agg$"Total.Collection.Size")
lib_df_agg$"Total.Operating.Income" <- as.numeric(lib_df_agg$"Total.Operating.Income")
lib_df_agg$"Municipal.Appropriation" <- as.numeric(lib_df_agg$"Municipal.Appropriation")
lib_df_agg$"Library.Materials.Expenditures" <- as.numeric(lib_df_agg$"Library.Materials.Expenditures")
lib_df_agg$"Wages...Salaries.Expenditures" <- as.numeric(lib_df_agg$"Wages...Salaries.Expenditures") 
lib_df_agg$"Operating.Expenditures" <- as.numeric(lib_df_agg$"Operating.Expenditures")

lib_df_agg$Town.Library <- NULL

lib_df_sum <- lib_df_agg %>% 
              group_by(Town, Fiscal.Year) %>% 
              summarise(sum_visits = sum(Total.Library.Visits), 
                        sum_borrowers = sum(Total.Registered.Borrowers), #no 0s
                        sum_ref_ques = sum(Reference.Questions),
                        sum_circ = sum(Total.Circulation),
                        sum_programs = sum(Total.Programs),
                        sum_attendance = sum(Total.Program.Attendance),
                        sum_internet = sum(Use.of.Public.Internet.Computers), #no 0s
                        sum_collection = sum(Total.Collection.Size), #no 0s
                        sum_op_income = sum(Total.Operating.Income),
                        sum_mun_app = sum(Municipal.Appropriation),
                        sum_lib_mat_exp = sum(Library.Materials.Expenditures), #no 0s
                        sum_wage_sal_exp = sum(Wages...Salaries.Expenditures),
                        sum_op_exp = sum(Operating.Expenditures)) %>% 
              complete(Town, Fiscal.Year)

#set 0 back to NAs
# lib_df_sum$sum_borrowers[lib_df_sum$sum_borrowers == 0] <- NA
# lib_df_sum$sum_internet[lib_df_sum$sum_internet == 0] <- NA
# lib_df_sum$sum_collection[lib_df_sum$sum_collection == 0] <- NA
# lib_df_sum$sum_lib_mat_exp[lib_df_sum$sum_lib_mat_exp == 0] <- NA




agg_test <- aggregate(. ~ `Town` + `Fiscal.Year`, lib_df_agg, sum, na.rm=T)                       
                        
                        
                        
                        
                        
                        



######POPULATION#######################################################################################
population <- lib_df_merge[,c("Fiscal Year", "Population of Service Area", "Town")]
population <- population[!is.na(population$`Fiscal Year`),]
population <- population[complete.cases(population),]
#remove duplicates
population<-population[!duplicated(population), ]

#add up population for multiple libraries
population$`Population of Service Area` <- as.numeric(population$`Population of Service Area`)
population <- aggregate(. ~ `Town` + `Fiscal Year`, population, sum)

years <- c("1996", "1997", "1998", "1999", "2000", "2001", "2002", 
           "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
           "2010", "2011", "2012", "2013", "2014","2015", "2016")

#backfill missing values (NAs as missing):
backfill <- expand.grid(
  `Town` = unique(lib_df_merge$Town),
  `Fiscal Year` = years 
)

#completed population df
population <- merge(population, backfill, by = c("Fiscal Year", "Town"), all.y = T)

######RANK#############################################################################################
rank <- lib_df_merge[,c("Fiscal Year", "AENGLC Wealth Rank", "Town")]
rank <- rank[complete.cases(rank),]
#remove duplicates
rank<-rank[!duplicated(rank), ]

#completed rank df
rank <- merge(rank, backfill, by = c("Fiscal Year", "Town"), all.y = T)

#remove 2nd rank row in any towns where there are two ranks present
rank <- rank[!duplicated(rank[,1:2]),]

#######################################################################################################

#Merge population and rank to start building final df
public_libraries <- merge(population, rank, by = c("Fiscal Year", "Town"))

######BACKFILL#########################################################################################
cols_to_agg <- lib_df_merge[,c(1,2,5:18)]
#set all "N/A" to NA
cols_to_agg[cols_to_agg == "N/A"] <- NA
#cols_to_agg[is.na(cols_to_agg$`Use of Public Internet Computers`)] <- 0
#remove rows with NAs (these will be backfilled later)
cols_to_sum <- cols_to_agg[complete.cases(cols_to_agg),]
aggregate_df <- data.frame(lapply(cols_to_sum, as.character), stringsAsFactors=FALSE)
aggregate_df$"Fiscal.Year" <- as.character(aggregate_df$"Fiscal.Year") 
aggregate_df$"AENGLC.Wealth.Rank" <- as.numeric(aggregate_df$"AENGLC.Wealth.Rank") 
aggregate_df$"Population.of.Service.Area" <- as.numeric(aggregate_df$"Population.of.Service.Area")
aggregate_df$"Total.Library.Visits" <- as.numeric(aggregate_df$"Total.Library.Visits")
aggregate_df$"Total.Registered.Borrowers" <- as.numeric(aggregate_df$"Total.Registered.Borrowers")
aggregate_df$"Reference.Questions" <- as.numeric(aggregate_df$"Reference.Questions")
aggregate_df$"Total.Circulation" <- as.numeric(aggregate_df$"Total.Circulation")
aggregate_df$"Total.Programs" <- as.numeric(aggregate_df$"Total.Programs")
aggregate_df$"Total.Program.Attendance" <- as.numeric(aggregate_df$"Total.Program.Attendance")
aggregate_df$"Use.of.Public.Internet.Computers" <- as.numeric(aggregate_df$"Use.of.Public.Internet.Computers")
aggregate_df$"Total.Collection.Size" <- as.numeric(aggregate_df$"Total.Collection.Size")
aggregate_df$"Total.Operating.Income" <- as.numeric(aggregate_df$"Total.Operating.Income")
aggregate_df$"Municipal.Appropriation" <- as.numeric(aggregate_df$"Municipal.Appropriation")
aggregate_df$"Library.Materials.Expenditures" <- as.numeric(aggregate_df$"Library.Materials.Expenditures")
aggregate_df$"Wages...Salaries.Expenditures" <- as.numeric(aggregate_df$"Wages...Salaries.Expenditures") 
aggregate_df$"Operating.Expenditures" <- as.numeric(aggregate_df$"Operating.Expenditures")

aggregate_df$Town.Library <- NULL
aggregate_df_sum <- aggregate(. ~ `Town` + `Fiscal.Year`, aggregate_df, sum)

cols_to_save <- cols_to_agg[!complete.cases(cols_to_agg),]
na_df <- data.frame(lapply(cols_to_save, as.character), stringsAsFactors=FALSE)
na_df$"Fiscal.Year" <- as.character(na_df$"Fiscal.Year") 
na_df$"AENGLC.Wealth.Rank" <- as.numeric(na_df$"AENGLC.Wealth.Rank") 
na_df$"Population.of.Service.Area" <- as.numeric(na_df$"Population.of.Service.Area")
na_df$"Total.Library.Visits" <- as.numeric(na_df$"Total.Library.Visits")
na_df$"Total.Registered.Borrowers" <- as.numeric(na_df$"Total.Registered.Borrowers")
na_df$"Reference.Questions" <- as.numeric(na_df$"Reference.Questions")
na_df$"Total.Circulation" <- as.numeric(na_df$"Total.Circulation")
na_df$"Total.Programs" <- as.numeric(na_df$"Total.Programs")
na_df$"Total.Program.Attendance" <- as.numeric(na_df$"Total.Program.Attendance")
na_df$"Use.of.Public.Internet.Computers" <- as.numeric(na_df$"Use.of.Public.Internet.Computers")
na_df$"Total.Collection.Size" <- as.numeric(na_df$"Total.Collection.Size")
na_df$"Total.Operating.Income" <- as.numeric(na_df$"Total.Operating.Income")
na_df$"Municipal.Appropriation" <- as.numeric(na_df$"Municipal.Appropriation")
na_df$"Library.Materials.Expenditures" <- as.numeric(na_df$"Library.Materials.Expenditures")
na_df$"Wages...Salaries.Expenditures" <- as.numeric(na_df$"Wages...Salaries.Expenditures") 
na_df$"Operating.Expenditures" <- as.numeric(na_df$"Operating.Expenditures")

na_df$Town.Library <- NULL

#bind aggregate and NA columns
complete_df <- rbind(na_df, aggregate_df_sum)

#remove "." from colnames
names(complete_df) <- gsub(x = names(complete_df),
                        pattern = "\\.",
                        replacement = " ")

#remove rows where all variables are NA

test <- complete_df[,c("Town", "Fiscal Year")]
test2<-test[duplicated(test), ]





lib_df_names_step_one <- lib_df_names[,-c(2,3)]
lib_df_names_step_one <- lib_df_names_step_one[complete.cases(lib_df_names_step_one),]



lib_df_names_step_two <- merge(lib_df_names_step_one, backfill, by = c("Fiscal Year", "Town"), all.y = T)






lib_df_names$Town.Library <- NULL
lib_complete <- lib_df_names[complete.cases(lib_df_names),]
lib_with_na <- lib_df_names[!complete.cases(lib_df_names),]
  
  
  as.data.frame(lib_df_names[!is.na(lib_df_names)])
lib_na <- lib_df_names[is.na(lib_df_names)]

lib_df_names <- data.frame(lapply(lib_df_names, as.character), stringsAsFactors=FALSE)

lib_df_names$"Fiscal.Year" <- as.character(lib_df_names$"Fiscal.Year") 
lib_df_names$"AENGLC.Wealth.Rank" <- as.numeric(lib_df_names$"AENGLC.Wealth.Rank") 
lib_df_names$"Population.of.Service.Area" <- as.numeric(lib_df_names$"Population.of.Service.Area")
lib_df_names$"Total.Library.Visits" <- as.numeric(lib_df_names$"Total.Library.Visits")
lib_df_names$"Total.Registered.Borrowers" <- as.numeric(lib_df_names$"Total.Registered.Borrowers")
lib_df_names$"Reference.Questions" <- as.numeric(lib_df_names$"Reference.Questions")
lib_df_names$"Total.Circulation" <- as.numeric(lib_df_names$"Total.Circulation")
lib_df_names$"Total.Programs" <- as.numeric(lib_df_names$"Total.Programs")
lib_df_names$"Total.Program.Attendance" <- as.numeric(lib_df_names$"Total.Program.Attendance")
lib_df_names$"Use.of.Public.Internet.Computers" <- as.numeric(lib_df_names$"Use.of.Public.Internet.Computers")
lib_df_names$"Total.Collection.Size" <- as.numeric(lib_df_names$"Total.Collection.Size")
lib_df_names$"Total.Operating.Income" <- as.numeric(lib_df_names$"Total.Operating.Income")
lib_df_names$"Municipal.Appropriation" <- as.numeric(lib_df_names$"Municipal.Appropriation")
lib_df_names$"Library.Materials.Expenditures" <- as.numeric(lib_df_names$"Library.Materials.Expenditures")
lib_df_names$"Wages...Salaries.Expenditures" <- as.numeric(lib_df_names$"Wages...Salaries.Expenditures") 
lib_df_names$"Operating.Expenditures" <- as.numeric(lib_df_names$"Operating.Expenditures")

lib_agg <- aggregate(. ~ `Town` + `Fiscal.Year`, lib_df_names, sum)

lib_agg <- lib_agg[lib_agg$Fiscal.Year != 0,]

years <- c("1996", "1997", "1998", "1999", "2000", "2001", "2002", 
           "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
           "2010", "2011", "2012", "2013", "2014","2015", "2016")

#backfill years
backfill <- expand.grid(
  `Town` = unique(lib_agg$Town),
  `Fiscal Year` = years 
)

total_library <- merge(lib_agg, backfill, by = c("Town", "Fiscal.Year"), all.y = T)

#set 0 back to NA
library_total[library_total == 0] <- NA
  
  
  
#set col names to first row
colnames(income_df2) <- income_df[2,] 

#Remove first 7 rows from income_df
income_df2 <- income_df2[-c(1:7),]

#Isolate columns needed from income_df for data set
income_df2 <- income_df2[,c(1,9)]



Variables:                                        Raw Columns:
-----------------------------------------------------------------------------------------------------
"AENGLC Rank"                                     | Straight Read: "AENGLC Wealth Rank"
"Circulation"                                     | Straight Read: "Total Circulation"
"Circulation per capita"                          | Calculated: "Total Circulation" / "Population"
"Collection Items per capita"                     | Straight Read: "Collection Size Per Capita"
"Collection Size"                                 | Straight Read: "Total Collection Size"
"Internet Computer Use"                           | Straight Read: "Use of Public Internet Computers" 
"Internet Use per capita"                         | Calculated: "Use of Public Internet Computers" / "Population"
"Library Materials Expenditure"                   | Straight Read: "Library Materials Expenditures"
"Library Materials Expenditure per capita"        | Calculated: "Library Materials Expenditures" / "Population"
"Library Visits"                                  | Straight Read: "Total Library Visits"
"Library Visits per capita"                       | Calculated: "Total Library Visits" / "Population"
"Municipal Appropriation"                         | Straight Read: "Municipal Appropriation" 
"Municipal Appropriation per capita"              | Calculated: "Municipal Appropriation" / "Population"
"Operating Expenditures"                          | Straight Read: "Operating Expenditures"   
"Operating Expenditures per capita"               | Straight Read: "Operating Expenditures Per Capita" 
"Operating Income"                                | Straight Read: "Total Operating Income" 
"Operating Income per capita"                     | Straight Read: "Operating Income Per Capita"  
"Program Attendance"                              | Straight Read: "Total Program Attendance" 
"Program Attendance per capita"                   | Calculated: "Total Program Attendance" / "Population"
"Programs"                                        | Straight Read: "Total Programs" 
"Reference Questions"                             | Straight Read: "Reference Questions"
"Reference Questions per capita"                  | Straight Read: "Reference Questions Per Capita"
"Registered Borrowers"                            | Straight Read: "Total Registered Borrowers"
"Registered Borrowers per capita"                 | Calculated: "Total Registered Borrowers" / "Population"
"State Appropriation"                             | Straight Read: "Income From State Funds"
"State Appropriation per capita"                  | Calculated: "Income From State Funds" / "Population"
"Wages and Salaries Expenditures"                 | Straight Read: "Wages & Salaries Expenditures"
"Wages and Salaries Expenditures per capita"      | Calculated: "Wages & Salaries Expenditures" / "Population"
  
Supporting Columns:
  "Population of Service Area"

