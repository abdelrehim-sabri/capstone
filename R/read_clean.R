#add date package
library(lubridate)
#library(dplyr)
#install.packages("reprex")
#library(reprex)
#library(ggplot2)


build_clean_dataset <- function() {
  datasetloc = "C:/Users/abdel/Desktop/Ryerson University/capstone/capstone/R/Health_Care_History.csv"
  if (file.exists(datasetloc)) {
    #alldata <- read.csv(file=datasetloc, stringsAsFactors = FALSE, header = T)
    alldata <- read.csv(file=datasetloc, header = T)
  }
  return(alldata)
}

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  
  return(calc.age)
}

get_age_group <- function(a) {
  ifelse(a<30,20, ifelse(a<40, 30, ifelse(a<50,40, ifelse(a<60, 50, ifelse(a<70, 60,70)))))
}

east_europe <- c('Ukraine','Russia','Poland','Czech Republic','Hungary')
west_europe <- c('Austria','Belgium','France','Germany','Italy','Netherlands','Portugal','Spain','Switzerland')
north_europe <- c('Sweden', 'Finland', 'Denmark')
british <- c('England','Scotland','Ireland')

get_ethnic_group <- function(country) {
  ifelse((country %in% east_europe), 'east_europe',
         ifelse((country %in% west_europe) ,'west_europe',
                ifelse((country %in% north_europe), 'north_europe',
                       ifelse((country %in% british), 'british',
                              country))))
}

get_binary_value <- function(value, compare_to) {
  ifelse(value==compare_to,1,0)
}

#Start cleaning up dataset
patients <- build_clean_dataset()
str(patients)

#fix the education column values and look for misspelled words
patients$education <- ifelse(patients$education == 'highscool', as.character('highschool'), as.character(patients$education))
patients$education <- ifelse(as.factor(patients$education) == 'phD/MD', as.character('phd/md'), as.character(patients$education))
patients$education <- as.factor(patients$education)

#group the ancestry countries to ethnic groups
patients$ancestry <- as.factor(get_ethnic_group(patients$ancestry))

patients$age <- age(patients$dob)
patients$age <- get_age_group(age(patients$dob))

#split the diseases into columns with binary values
#patients$cancers <- get_binary_value(patients$disease,'cancer')
#patients$diabetes <- get_binary_value(patients$disease,'diabetes')
#patients$heart_disease <- get_binary_value(patients$disease,'heart disease')
#patients$hypertension <- get_binary_value(patients$disease,'hypertension')
#patients$endometriosis <- get_binary_value(patients$disease,'endometriosis')
#patients$multiple_sclerosis <- get_binary_value(patients$disease,'multiple sclerosis')
#patients$schizophrenia <- get_binary_value(patients$disease,'schizophrenia')
#patients$kidney_disease <- get_binary_value(patients$disease,'kidney disease')
#patients$gastritis <- get_binary_value(patients$disease,'gastritis')
#patients$alzheimer <- get_binary_value(patients$disease,'alzheimer')

#draw a bar plot to count the total number of diseases in the dataset
counts <- table(patients$disease)

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.

barplot(sort(counts, decreasing = TRUE), main="Disease Names", 
        xlab="Diseases Frequency", 
        col="darkblue",
        horiz=TRUE,
        cex.names=0.8,
        xlim = c(0, 350))