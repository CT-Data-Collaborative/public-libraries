library(plyr)
library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)
library(magrittr)

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
path_to_income_data <- (paste0(path_to_raw_data, "/", "for_Income"))
hist_lib_xlsx <- dir(path_to_raw_data,  pattern = "CTPublicLibraries") 
income_xlsx <- dir(path_to_income_data, pattern = "Profile_") 
hist_lib_df <- (read_excel(paste0(path_to_raw_data, "/", hist_lib_xlsx), sheet=1, skip=1, col_types = "text"))

lib_town_xwalk <- read.csv(paste0(path_to_raw_data, "/", "library_town_crosswalk_complete.csv"), stringsAsFactors = F, header = T)

#Isolate columns needed from lib_df for data set
hist_lib_df <- hist_lib_df[,c("Use the Filter Tool to Choose Your Library", 
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
hist_lib_df <- hist_lib_df[-1,]

#rename library name column
names(hist_lib_df)[names(hist_lib_df) == "Use the Filter Tool to Choose Your Library"] <- "Town.Library"
lib_df_merge <- merge(hist_lib_df, lib_town_xwalk, by = "Town.Library", all=T)

#fix manual N/As
lib_df_merge[lib_df_merge == "N/A"] <- NA

#create df for aggregation
lib_df_agg <- data.frame(lapply(lib_df_merge, as.character), stringsAsFactors=FALSE)

#convert certain columns to numeric (so they can be summed)
for (i in 3:17) {
  lib_df_agg[,i] <- as.numeric(as.character(lib_df_agg[,i]))           
}

#remove library names
lib_df_agg$Town.Library <- NULL

#Set "Town" to the first column
x <- "Town"
lib_df_agg <- lib_df_agg[c(x, setdiff(names(lib_df_agg), x))]

#remove rows where year=NA
lib_df_agg <- lib_df_agg[!is.na(lib_df_agg$Fiscal.Year),]

#create separate population column
lib_df_pop <- lib_df_agg[,c(1,2,4)]
lib_df_pop[is.na(lib_df_pop)] <- 0
lib_df_pop <- lib_df_pop %>% 
  group_by(Town, Fiscal.Year) %>%   
  summarise(Population.of.Service.Area = sum(Population.of.Service.Area))
lib_df_pop <- as.data.frame(lib_df_pop)

#create separate rank column
lib_df_rank <- lib_df_agg[,c(1,2,3)]
#remove NAs
lib_df_rank <- lib_df_rank[!is.na(lib_df_rank$AENGLC.Wealth.Rank),]
#remove duplicates
lib_df_rank<-lib_df_rank[!duplicated(lib_df_rank),]
lib_df_rank <- lib_df_rank %>% 
  group_by(Town, Fiscal.Year) %>% 
  summarise(AENGLC.Wealth.Rank = max(as.numeric(AENGLC.Wealth.Rank))) #for any towns that have different ranks, take the max rank
lib_df_rank <- as.data.frame(lib_df_rank)

lib_df_sum <- lib_df_agg %>% 
  group_by(Town, Fiscal.Year) %>% 
  summarise(Total.Library.Visits = sum(Total.Library.Visits, na.rm=T), 
            Total.Registered.Borrowers = sum(Total.Registered.Borrowers, na.rm=T), 
            Reference.Questions = sum(Reference.Questions, na.rm=T),
            Total.Circulation = sum(Total.Circulation, na.rm=T),
            Total.Programs = sum(Total.Programs, na.rm=T),
            Total.Program.Attendance = sum(Total.Program.Attendance, na.rm=T),
            Use.of.Public.Internet.Computers = sum(Use.of.Public.Internet.Computers, na.rm=T), 
            Total.Collection.Size = sum(Total.Collection.Size, na.rm=T), 
            Total.Operating.Income = sum(Total.Operating.Income, na.rm=T),
            Municipal.Appropriation = sum(Municipal.Appropriation, na.rm=T),
            Library.Materials.Expenditures = sum(Library.Materials.Expenditures, na.rm=T), 
            Wages...Salaries.Expenditures = sum(Wages...Salaries.Expenditures, na.rm=T),
            Operating.Expenditures = sum(Operating.Expenditures, na.rm=T)) %>% 
  complete(Town, Fiscal.Year)

#set all 0s to NAs
lib_df_sum <- as.data.frame(lib_df_sum)
lib_df_sum[lib_df_sum == 0] <- NA

total_sum <- join_all(list(lib_df_pop, lib_df_rank, lib_df_sum), by = c("Town", "Fiscal.Year"), type = 'full')

#bring in State Appropriation columns
#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

cols <- c("income", "librarydata")
for (i in 1:length(income_xlsx)) {
  mysheets <- read_excel_allsheets(paste0(path_to_income_data, "/", income_xlsx[i]))
  income_sheet_index <- grep(paste(cols, collapse="|"), names(mysheets), value=T, ignore.case=T)
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(income_xlsx[i])), "")), 1, 4))
  current_sheet_file <- mysheets[[income_sheet_index]] 
  assign(paste0("Income_", get_year), current_sheet_file)
}

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
income_data <- grep("Income", dfs, value=T)

Income_2009 <- Income_2009[-c(3:4)] #quick fix so all years columns line up
Income_2017 <- Income_2017[-c(3:224)] #quick fix so all years columns line up

#Extract Income columns from each year
income_all_years <- data.frame(stringsAsFactors = F)
for (i in 1:length(income_data)) {
  current_file <- get(income_data[i])
  current_file <- current_file[,c(1,9)]
  colnames(current_file) <- c("Town.Library", "State Appropriation")
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(income_data[i])), "")), 1, 4))
  current_file$Fiscal.Year <- get_year
  current_file <- current_file[-c(1:7),]
  income_all_years <- rbind(income_all_years, current_file)
}

#merge in xwalk file
income_all_years_merge <- merge(income_all_years, lib_town_xwalk, by = "Town.Library", all.y = T)

#remove rows where year is NA
income_all_years_merge <- income_all_years_merge[!is.na(income_all_years_merge$Fiscal.Year),]

income_all_years_merge$Town.Library <- NULL

income_all_years_merge_sum <- income_all_years_merge %>% 
  group_by(Town, Fiscal.Year) %>%   
  summarise(State.Appropriation = sum(as.numeric(`State Appropriation`)))

#backfill towns and years
years <- c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", 
           "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

backfill <- expand.grid (
  `Fiscal.Year` = years,
  `Town` = unique(income_all_years_merge_sum$Town)
)

complete_state_appr <- merge(income_all_years_merge_sum, backfill, by = c("Town", "Fiscal.Year"), all.y=T)

complete_total <- merge(total_sum, backfill, by = c("Town", "Fiscal.Year"), all.y=T)

#bind state appropriation with rest of df
public_lib <- merge(complete_total, complete_state_appr, by = c("Town", "Fiscal.Year"))


######################################################################################################

#Rename columns
public_lib <- plyr::rename(public_lib, c("Fiscal.Year"="Year",                
                                         "Population.of.Service.Area"="Population of Service Area",
                                         "AENGLC.Wealth.Rank"="AENGLC Rank",
                                         "Total.Library.Visits"="Library Visits",
                                         "Total.Registered.Borrowers"="Registered Borrowers",
                                         "Reference.Questions"="Reference Questions",
                                         "Total.Circulation"="Circulation",
                                         "Total.Programs"="Programs",
                                         "Total.Program.Attendance"="Program Attendance",
                                         "Use.of.Public.Internet.Computers"="Internet Computer Use",
                                         "Total.Collection.Size"="Collection Size",
                                         "Total.Operating.Income"="Operating Income",
                                         "Municipal.Appropriation"="Municipal Appropriation",
                                         "Library.Materials.Expenditures"="Library Materials Expenditures",
                                         "Wages...Salaries.Expenditures"="Wages and Salaries Expenditures",
                                         "Operating.Expenditures"="Operating Expenditures",
                                         "State.Appropriation"="State Appropriation"))

#Calculated columns
public_lib$"Circulation per capita"                     <- public_lib$"Circulation" / public_lib$"Population of Service Area"
public_lib$"Collection Size per capita"                 <- public_lib$"Collection Size" / public_lib$"Population of Service Area"
public_lib$"Internet Computer Use per capita"           <- public_lib$"Internet Computer Use" / public_lib$"Population of Service Area"
public_lib$"Library Materials Expenditures per capita"  <- public_lib$"Library Materials Expenditures" / public_lib$"Population of Service Area"
public_lib$"Library Visits per capita"                  <- public_lib$"Library Visits" / public_lib$"Population of Service Area"
public_lib$"Municipal Appropriation per capita"         <- public_lib$"Municipal Appropriation" / public_lib$"Population of Service Area"
public_lib$"Operating Expenditures per capita"          <- public_lib$"Operating Expenditures" / public_lib$"Population of Service Area"
public_lib$"Operating Income per capita"                <- public_lib$"Operating Income" / public_lib$"Population of Service Area"
public_lib$"Program Attendance per capita"              <- public_lib$"Program Attendance" / public_lib$"Population of Service Area"
public_lib$"Reference Questions per capita"             <- public_lib$"Reference Questions" / public_lib$"Population of Service Area"
public_lib$"Registered Borrowers per capita"            <- public_lib$"Registered Borrowers" / public_lib$"Population of Service Area"
public_lib$"State Appropriation per capita"             <- public_lib$"State Appropriation" / public_lib$"Population of Service Area"
public_lib$"Wages and Salaries Expenditures per capita" <- public_lib$"Wages and Salaries Expenditures" / public_lib$"Population of Service Area"

#remove Population
public_lib$`Population of Service Area` <- NULL

#Convert to Long format
cols_to_stack <- c("AENGLC Rank",                               
                   "Library Visits", 
                   "Library Visits per capita",                  
                   "Registered Borrowers",
                   "Registered Borrowers per capita",            
                   "Reference Questions", 
                   "Reference Questions per capita",            
                   "Circulation",         
                   "Circulation per capita", 
                   "Programs",            
                   "Program Attendance",   
                   "Program Attendance per capita",              
                   "Internet Computer Use", 
                   "Internet Computer Use per capita",                    
                   "Collection Size",       
                   "Collection Size per capita",
                   "Operating Income",      
                   "Operating Income per capita",                
                   "Municipal Appropriation", 
                   "Municipal Appropriation per capita",  
                   "State Appropriation",     
                   "State Appropriation per capita",
                   "Operating Expenditures",  
                   "Operating Expenditures per capita",                   
                   "Library Materials Expenditures", 
                   "Library Materials Expenditures per capita",  
                   "Wages and Salaries Expenditures", 
                   "Wages and Salaries Expenditures per capita")           

long_row_count = nrow(public_lib) * length(cols_to_stack)

public_lib_long <- reshape(public_lib,
                           varying = cols_to_stack,
                           v.names = "Value",
                           timevar = "Variable",
                           times = cols_to_stack,
                           new.row.names = 1:long_row_count,
                           direction = "long"
)

#sort columns, remove ID column, round Value column
public_lib_long <- public_lib_long %>% 
  select(Town, Year, Variable, Value) %>% 
  arrange(Town, Year) %>% 
  mutate(Value = round(Value, 2))

#Recode missing values
public_lib_long[is.na(public_lib_long)] <- -6666

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

public_lib_long_fips <- merge(public_lib_long, fips, by = "Town", all=T)

#remove CT and four towns with no libraries listed
public_lib_long_fips <- public_lib_long_fips[!is.na(public_lib_long_fips$Year),]

#Assign Measure Type
public_lib_long_fips$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
public_lib_long_fips$"Measure Type"[which(public_lib_long_fips$Variable %in% c("AENGLC Rank"))] <- "Rank"
public_lib_long_fips$"Measure Type"[which(public_lib_long_fips$Variable %in% c("Circulation",
                                                                               "Circulation per capita",
                                                                               "Collection Size per capita",
                                                                               "Collection Size",
                                                                               "Internet Computer Use",
                                                                               "Internet Computer Use per capita",
                                                                               "Library Visits",
                                                                               "Library Visits per capita",
                                                                               "Program Attendance",
                                                                               "Program Attendance per capita",
                                                                               "Programs",
                                                                               "Reference Questions",
                                                                               "Reference Questions per capita",
                                                                               "Registered Borrowers",
                                                                               "Registered Borrowers per capita"))] <- "Number"
public_lib_long_fips$"Measure Type"[which(public_lib_long_fips$Variable %in% c("Library Materials Expenditures",
                                                                               "Library Materials Expenditures per capita",
                                                                               "Municipal Appropriation",
                                                                               "Municipal Appropriation per capita",
                                                                               "Operating Expenditures",
                                                                               "Operating Expenditures per capita",
                                                                               "Operating Income",
                                                                               "Operating Income per capita",
                                                                               "State Appropriation",
                                                                               "State Appropriation per capita",
                                                                               "Wages and Salaries Expenditures",
                                                                               "Wages and Salaries Expenditures per capita"))] <- "Currency"

#Order columns
public_lib_long_fips <- public_lib_long_fips %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable,  Value) %>% 
  arrange(Town, Year, `Measure Type`, Variable)

# Write to File
write.table(
  public_lib_long_fips,
  file.path(getwd(), "data", "public_libraries_1996_2017.csv"),
  sep = ",",
  row.names = F
)
