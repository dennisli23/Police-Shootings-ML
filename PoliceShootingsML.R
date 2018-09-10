#' ---
#' title: "Stats 101C (Data Mining and Statistical Learning) Final Project"
#' author: "Dennis Li, Jefferson Yee, Danielle Sim"
#' date: "May 31, 2018"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' #Libraries
## ---- message=FALSE, warning=FALSE---------------------------------------
library(stringr)
library(readxl)
library(dplyr)
library(tree)
library(randomForest)

#' 
#' #Reading In data
## ------------------------------------------------------------------------
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

#external data on median household income by city
income <- read_excel("medianhouseholdincome.xlsx")

#external data on black population percentage by city
blackpop <- read_excel("blackpop.xlsx")
violentcrime = read_excel("ViolentCrime.xlsx")

samplesubmission <- read.csv("samplesubmission2.csv")
officerprop = read_excel("OfficersProp.xlsx")


#' 
#' #SubjectAge
## ------------------------------------------------------------------------
#train
train$SubjectAge = as.character(train$SubjectAge)
for(i in 1:nrow(train)){
  if(train$SubjectAge[i] == "U" | train$SubjectAge[i] == "UNKNOWN" | train$SubjectAge[i] == "N/A")
    train$SubjectAge[i] = NA
  else if(train$SubjectAge[i] == "Juvenile")
    train$SubjectAge[i] = "18"
  else if(str_detect(train$SubjectAge[i], "/."))
    train$SubjectAge[i] = ((train$SubjectAge[i]))
  else if(str_detect(train$SubjectAge[i], "-")){
    low = as.numeric(gsub("-..","",train$SubjectAge[i]))
    high = as.numeric(gsub("[0-9]*-","",train$SubjectAge[i]))
    train$SubjectAge[i] = as.character(mean(c(low, high)))
  }
  else next
}
train$SubjectAge = as.numeric(train$SubjectAge)

#test
for(i in 1:nrow(test)){
  if(test$SubjectAge[i] == "U" | test$SubjectAge[i] == "UNKNOWN" | test$SubjectAge[i] == "N/A")
    test$SubjectAge[i] = NA
  else if(test$SubjectAge[i] == "Juvenile")
    test$SubjectAge[i] = "18"
  else if(str_detect(test$SubjectAge[i], "/."))
    test$SubjectAge[i] = ((test$SubjectAge[i]))
  else if(str_detect(test$SubjectAge[i], "-")){
    low = as.numeric(gsub("-..","",test$SubjectAge[i]))
    high = as.numeric(gsub("[0-9]*-","",test$SubjectAge[i]))
    test$SubjectAge[i] = as.character(mean(c(low, high)))
  }
  else next
}
test$SubjectAge = as.numeric(test$SubjectAge)


mean_age <- mean(train$SubjectAge, na.rm = TRUE)
for(i in 1:nrow(train)){
  if(is.na(train$SubjectAge[i]))
    train$SubjectAge[i] <- mean_age
}

mean_age <- mean(test$SubjectAge, na.rm = TRUE)
for(i in 1:nrow(test)){
  if(is.na(test$SubjectAge[i]))
    test$SubjectAge[i] <- mean_age
}


#' 
#' 
#' #normalize NatureofSTOP
## ------------------------------------------------------------------------

train$NatureOfStop = tolower(train$NatureOfStop)
#train$NatureOfStop = factor(train$NatureOfStop)

test$NatureOfStop = tolower(test$NatureOfStop)
#test$NatureOfStop = factor(test$NatureOfStop)

#stop_levels <- levels(factor(train$NatureOfStop))

for(i in 1:nrow(train)){

  if(is.na(train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "missing"
  }
  else if(grepl("armed", train$NatureOfStop[i]) |
    grepl("firearm", train$NatureOfStop[i]) |
    grepl("gun", train$NatureOfStop[i]) |
    grepl("shoot", train$NatureOfStop[i]) |
    grepl("weapon", train$NatureOfStop[i]) |
    grepl("hostage", train$NatureOfStop[i]) |
    grepl("kidnap", train$NatureOfStop[i]) |
    grepl("shot", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "gun"
  }

  else if(grepl("accidental", train$NatureOfStop[i]) |
          grepl("911", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "accident"
  }

  else if(grepl("arrest", train$NatureOfStop[i]) |
          grepl("fugitive", train$NatureOfStop[i]) |
          grepl("warrant", train$NatureOfStop[i]) |
          grepl("flagged", train$NatureOfStop[i]) |
          grepl("wanted", train$NatureOfStop[i]) |
          grepl("suspect", train$NatureOfStop[i]))
  {
      train$NatureOfStop[i] <- "arrest"
  }

  else if(grepl("assault", train$NatureOfStop[i]) |
          grepl("battery", train$NatureOfStop[i]) |
          grepl("ambush", train$NatureOfStop[i]) |
          grepl("violence", train$NatureOfStop[i]) |
          grepl("fight", train$NatureOfStop[i]) |
          grepl("homicide", train$NatureOfStop[i]) |
          grepl("murder", train$NatureOfStop[i]) |
          grepl("stab", train$NatureOfStop[i]) |
          grepl("suicid", train$NatureOfStop[i]) |
          grepl("violence", train$NatureOfStop[i]) |
          grepl("violent", train$NatureOfStop[i]))
  {
     train$NatureOfStop[i] <- "violence"
  }

  else if(grepl("robber", train$NatureOfStop[i]) |
          grepl("bulgary", train$NatureOfStop[i]) |
          grepl("breaking", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "robbery"
  }

  else if(grepl("bolo", train$NatureOfStop[i]) |
          grepl("suspicious", train$NatureOfStop[i]) |
          grepl("gang", train$NatureOfStop[i]) |
          grepl("check", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "suspicious"
  }

  else if(grepl("car", train$NatureOfStop[i]) |
          grepl("vehicle", train$NatureOfStop[i]) |
          grepl("driver", train$NatureOfStop[i]) |
          grepl("d.u.i.", train$NatureOfStop[i]) |
          grepl("traffic", train$NatureOfStop[i]) |
          grepl("hit", train$NatureOfStop[i]) |
          grepl("road", train$NatureOfStop[i]) |
          grepl("stolen", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "car"
  }

  else if(grepl("disturb", train$NatureOfStop[i]) |
          grepl("noise", train$NatureOfStop[i]) |
          grepl("party", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "disturbance"
  }

  else if(grepl("drug", train$NatureOfStop[i]) |
          grepl("narcotics", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "drug"
  }

  else if(grepl("off-duty", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "off-duty"
  }
  else if(grepl("mental", train$NatureOfStop[i]))
  {
    train$NatureOfStop[i] <- "mental"
  }
  else
    train$NatureOfStop[i] <- "other"

}


stop_levels <- levels(factor(train$NatureOfStop))




for(i in 1:nrow(test)){

  if(is.na(test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "missing"
  }
  else if(grepl("armed", test$NatureOfStop[i]) |
    grepl("firearm", test$NatureOfStop[i]) |
    grepl("gun", test$NatureOfStop[i]) |
    grepl("shoot", test$NatureOfStop[i]) |
    grepl("weapon", test$NatureOfStop[i]) |
    grepl("hostage", test$NatureOfStop[i]) |
    grepl("kidnap", test$NatureOfStop[i]) |
    grepl("shot", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "gun"
  }

  else if(grepl("accidental", test$NatureOfStop[i]) |
          grepl("911", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "accident"
  }

  else if(grepl("arrest", test$NatureOfStop[i]) |
          grepl("fugitive", test$NatureOfStop[i]) |
          grepl("warrant", test$NatureOfStop[i]) |
          grepl("flagged", test$NatureOfStop[i]) |
          grepl("wanted", test$NatureOfStop[i]) |
          grepl("suspect", test$NatureOfStop[i]))
  {
      test$NatureOfStop[i] <- "arrest"
  }

  else if(grepl("assault", test$NatureOfStop[i]) |
          grepl("battery", test$NatureOfStop[i]) |
          grepl("ambush", test$NatureOfStop[i]) |
          grepl("violence", test$NatureOfStop[i]) |
          grepl("fight", test$NatureOfStop[i]) |
          grepl("homicide", test$NatureOfStop[i]) |
          grepl("murder", test$NatureOfStop[i]) |
          grepl("stab", test$NatureOfStop[i]) |
          grepl("suicid", test$NatureOfStop[i]) |
          grepl("violence", test$NatureOfStop[i]) |
          grepl("violent", test$NatureOfStop[i]))
  {
     test$NatureOfStop[i] <- "violence"
  }

  else if(grepl("robber", test$NatureOfStop[i]) |
          grepl("bulgary", test$NatureOfStop[i]) |
          grepl("breaking", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "robbery"
  }

  else if(grepl("bolo", test$NatureOfStop[i]) |
          grepl("suspicious", test$NatureOfStop[i]) |
          grepl("gang", test$NatureOfStop[i]) |
          grepl("check", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "suspicious"
  }

  else if(grepl("car", test$NatureOfStop[i]) |
          grepl("vehicle", test$NatureOfStop[i]) |
          grepl("driver", test$NatureOfStop[i]) |
          grepl("d.u.i.", test$NatureOfStop[i]) |
          grepl("traffic", test$NatureOfStop[i]) |
          grepl("hit", test$NatureOfStop[i]) |
          grepl("road", test$NatureOfStop[i]) |
          grepl("stolen", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "car"
  }

  else if(grepl("disturb", test$NatureOfStop[i]) |
          grepl("noise", test$NatureOfStop[i]) |
          grepl("party", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "disturbance"
  }

  else if(grepl("drug", test$NatureOfStop[i]) |
          grepl("narcotics", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "drug"
  }

  else if(grepl("off-duty", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "off-duty"
  }
  else if(grepl("mental", test$NatureOfStop[i]))
  {
    test$NatureOfStop[i] <- "mental"
  }
  else
    test$NatureOfStop[i] <- "other"

}




#' 
#' 
#' #Officer Races and gender
## ---- warning=FALSE, message=FALSE---------------------------------------


for(i in 1:nrow(train)){
  #Train Officer Race
  full_string <- as.character(train$OfficerRace[i])
  races <- unlist(strsplit(full_string, ";"))

  if(is.na(races[1])){
    train$OfficerRace[i] <- 0
  }
  else{
    train$OfficerRace[i] <- length(which(races == "W"))/length(races)
  }

  #Train Officer Gender
  gender_string <- as.character(train$OfficerGender[i])
  gender <- unlist(strsplit(gender_string, ";"))

  if(is.na(gender)){
    train$Gender[i] <- NA
  }
  else{
    train$OfficerGender[i] <- length(which(gender == "M"))/length(gender)
  }

}

for(i in 1:1400){

  #Test Officer Race
  full_string_test <- as.character(test$OfficerRace[i])
  races_test <- unlist(strsplit(full_string_test, ";"))

  if(is.na(races[1])){
    test$OfficerRace[i] <- 0
  }
  else{
    test$OfficerRace[i] <- length(which(races == "W"))/length(races)
  }

  #Test Officer Gender
  gender_string <- as.character(test$OfficerGender[i])
  gender <- unlist(strsplit(gender_string, ";"))

  if(is.na(gender)){
    test$Gender[i] <- NA
  }
  else{
    test$OfficerGender[i] <- length(which(gender == "M"))/length(gender)
  }

}


#' 
#' 
#' 
#' #Number of Shots
## ------------------------------------------------------------------------

for(i in 1:nrow(train)){
  if(is.na(train$NumberOfShots[i])) next
  else if(str_detect(train$NumberOfShots[i], ">")){
    train$NumberOfShots[i] = str_extract(train$NumberOfShots[i], "[0-9]+")
  }
 else if(str_detect(train$NumberOfShots[i], ";")){
   train$NumberOfShots[i] = sum(as.numeric(str_extract_all(train$NumberOfShots[i], "[0-9]+")[[1]]))
 }
  else if(train$NumberOfShots[i] == "not clear" || train$NumberOfShots[i] == "U" ||
          train$NumberOfShots[i] == "Unknown" || train$NumberOfShots[i] == "no information")
    train$NumberOfShots[i] = NA
  else if(train$NumberOfShots[i] == "Multiple")
    train$NumberOfShots[i] = 2
  else if(str_detect(train$NumberOfShots[i], "total"))
    train$NumberOfShots[i] = str_extract(train$NumberOfShots[i], "[0-9]+")
  else next
}
train$NumberOfShots[901] = "1"
train$NumberOfShots = as.numeric(train$NumberOfShots)

for(i in 1:nrow(test)){
  if(is.na(test$NumberOfShots[i])) next
  else if(str_detect(test$NumberOfShots[i], ">")){
    test$NumberOfShots[i] = str_extract(test$NumberOfShots[i], "[0-9]+")
  }
  else if(str_detect(test$NumberOfShots[i], ";")){
    test$NumberOfShots[i] = sum(as.numeric(str_extract_all(test$NumberOfShots[i], "[0-9]+")[[1]]))
  }
  else if(test$NumberOfShots[i] == "not clear" || test$NumberOfShots[i] == "U" ||
          test$NumberOfShots[i] == "Unknown" || test$NumberOfShots[i] == "no information")
    test$NumberOfShots[i] = NA
  else if(test$NumberOfShots[i] == "Multiple")
    test$NumberOfShots[i] = 2
  else if(str_detect(test$NumberOfShots[i], "total"))
    test$NumberOfShots[i] = str_extract(test$NumberOfShots[i], "[0-9]+")
  else next
}

test$NumberOfShots = as.numeric(test$NumberOfShots)

median_shots <- median(train$NumberOfShots, na.rm=TRUE)
for(i in 1:nrow(train)){
  if(is.na(train$NumberOfShots[i]))
    train$NumberOfShots[i] <- median_shots
}


median_shots <- median(test$NumberOfShots, na.rm=TRUE)
for(i in 1:nrow(test)){
  if(is.na(test$NumberOfShots[i]))
    test$NumberOfShots[i] <- median_shots
}


#' 
#' 
#' 
#' #Merging external data
## ------------------------------------------------------------------------

train <- merge(train, blackpop, by = "City")
train <- merge(train, income, by = "City")
train = merge(train, violentcrime, by = "City")
train = merge(train, officerprop, by = "City")
test <- merge(test, blackpop, by = "City")
test <- merge(test, income, by = "City")
test = merge(test, violentcrime, by = "City")
test = merge(test, officerprop, by = "City")


#' 
#' 
#' #Full Narrative
## ------------------------------------------------------------------------

train$FullNarrative <- tolower(train$FullNarrative)
test$FullNarrative <- tolower(test$FullNarrative)


for(i in 1:nrow(train)){
  if(is.na(train$FullNarrative[i]))
    train$FullNarrative[i] <- 0
  else
    train$FullNarrative[i] <- 1
}

for(i in 1:nrow(test)){
  if(is.na(test$FullNarrative[i]))
    test$FullNarrative[i] <- 0
  else
    test$FullNarrative[i] <- 1
}


#' 
#' 
#' #Notes and Newsstory?
## ------------------------------------------------------------------------

train$Notes <- tolower(train$Notes)
test$Notes <- tolower(test$Notes)

#levels(factor(train$Notes))


#Newsstory

train$News <- ifelse(grepl("http:", train$Notes), 1, 0)
test$News <- ifelse(grepl("http:", test$Notes), 1, 0)


for(i in 1:nrow(train)){
  if(is.na(train$Notes[i]))
    train$Notes[i] <- "None"
  
  else if(grepl("no hit", train$Notes[i]) |
          grepl("miss", train$Notes[i]))
    train$Notes[i] <- "miss"
  
   else if(#grepl("hit", train$Notes[i]) |
           grepl("deadly weapon", train$Notes[i]) |
           grepl("wound", train$Notes[i]) |
           grepl("death", train$Notes[i]) |
           grepl("shooting", train$Notes[i]) |
           grepl("stab", train$Notes[i]) |
           grepl("attack", train$Notes[i]) |
           grepl("suicide", train$Notes[i]) |
           grepl("dead", train$Notes[i]) |
           grepl("crash", train$Notes[i]) |
           grepl("fatal", train$Notes[i]) |
           grepl("kill", train$Notes[i]))
    train$Notes[i] <- "hit"
   
   else if(grepl("weapon", train$Notes[i]) | 
           grepl("knife", train$Notes[i]) |
           grepl("rifle", train$Notes[i]) |
           grepl("bayonet", train$Notes[i]) |
           grepl("pistol", train$Notes[i]) |
           grepl("revolver", train$Notes[i]) | 
           grepl("firearm", train$Notes[i]) |
           grepl("gun", train$Notes[i]) |
           grepl("knives", train$Notes[i]) |
           grepl("armed", train$Notes[i]) |
           grepl("axe", train$Notes[i]) |
           grepl("bat", train$Notes[i]) |
           grepl("instrument", train$Notes[i]) |
           grepl("blunt object", train$Notes[i]) |
           grepl("hammer", train$Notes[i]) |
           grepl("scissor", train$Notes[i]) |
           grepl("grenade", train$Notes[i]) |
           grepl("shotgun", train$Notes[i]) |
           grepl("sword", train$Notes[i]) |
           grepl("mace", train$Notes[i]) |
           grepl("machete", train$Notes[i]) |
           grepl("pipe", train$Notes[i]) |
           grepl("screwdriver", train$Notes[i]))
    train$Notes[i] <- "weapon"
   
   else if(grepl("accident", train$Notes[i]))
     train$Notes[i] <- "accident"
   
   else 
     train$Notes[i] <- "other"
}

levels(factor(train$Notes))


for(i in 1:nrow(test)){
  if(is.na(test$Notes[i]))
    test$Notes[i] <- "None"
  
  else if(grepl("no hit", test$Notes[i]) |
          grepl("miss", test$Notes[i]))
    test$Notes[i] <- "miss"
  
   else if(grepl("hit", test$Notes[i]) |
           grepl("deadly weapon", test$Notes[i]) |
           grepl("wound", test$Notes[i]) |
           grepl("death", test$Notes[i]) |
           grepl("shooting", test$Notes[i]) |
           grepl("stab", test$Notes[i]) |
           grepl("attack", test$Notes[i]) |
           grepl("suicide", test$Notes[i]) |
           grepl("dead", test$Notes[i]) |
           grepl("crash", test$Notes[i]) |
           grepl("fatal", test$Notes[i]) |
           grepl("kill", test$Notes[i]))
    test$Notes[i] <- "hit"
   
   else if(grepl("weapon", test$Notes[i]) | 
           grepl("knife", test$Notes[i]) |
           grepl("rifle", test$Notes[i]) |
           grepl("bayonet", test$Notes[i]) |
           grepl("pistol", test$Notes[i]) |
           grepl("revolver", test$Notes[i]) | 
           grepl("firearm", test$Notes[i]) |
           grepl("gun", test$Notes[i]) |
           grepl("knives", test$Notes[i]) |
           grepl("armed", test$Notes[i]) |
           grepl("axe", test$Notes[i]) |
           grepl("bat", test$Notes[i]) |
           grepl("instrument", test$Notes[i]) |
           grepl("blunt object", test$Notes[i]) |
           grepl("hammer", test$Notes[i]) |
           grepl("scissor", test$Notes[i]) |
           grepl("grenade", test$Notes[i]) |
           grepl("shotgun", test$Notes[i]) |
           grepl("sword", test$Notes[i]) |
           grepl("mace", test$Notes[i]) |
           grepl("machete", test$Notes[i]) |
           grepl("pipe", test$Notes[i]) |
           grepl("screwdriver", test$Notes[i]))
    test$Notes[i] <- "weapon"
   
   else if(grepl("accident", test$Notes[i]))
     test$Notes[i] <- "accident"
   
   else 
     test$Notes[i] <- "other"
}



#' 
#' 
#' 
#' #Misc
## ------------------------------------------------------------------------

train$SubjectRace[2462] = "B"
test$SubjectGender[1256] = "M"

for(i in 1:nrow(train)){
  if(is.na(train$SubjectArmed[i]))
    train$SubjectArmed[i] <- "U"
  
  if(is.na(train$NumberOfOfficers[i]))
    train$NumberOfOfficers[i] <- mean(train$NumberOfOfficers, na.rm = TRUE)
  
  if(is.na(train$LawEnforcementOfficersProp[i]))
    train$LawEnforcementOfficersProp[i] <- mean(train$LawEnforcementOfficersProp, na.rm = TRUE)
}

for(i in 1:nrow(test)){
  if(is.na(test$SubjectArmed[i]))
    test$SubjectArmed[i] <- "U"
  
  if(is.na(test$NumberOfOfficers[i]))
    test$NumberOfOfficers[i] <- mean(test$NumberOfOfficers, na.rm = TRUE)
  
  if(is.na(test$LawEnforcementOfficersProp[i]))
    test$LawEnforcementOfficersProp[i] <- mean(test$LawEnforcementOfficersProp, na.rm = TRUE)
}





#' 
#' #Dates
#' 
## ------------------------------------------------------------------------


train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

for(i in 1:nrow(train)){
  if(is.na(train$Date[i]))
    train$Date[i] <- median(train$Date, na.rm = TRUE)
}

for(i in 1:nrow(test)){
  if(is.na(test$Date[i]))
    test$Date[i] <- median(test$Date, na.rm = TRUE)
}

  

#' 
#' 
#' #Numeric
## ------------------------------------------------------------------------

train <- train[-which(train$Fatal == "U"), ]

train$OfficerRace <- as.numeric(train$OfficerRace)
train$OfficerGender <- as.numeric(train$OfficerGender)

train$City <- as.factor(train$City)
train$Fatal <- as.factor(train$Fatal)
train$SubjectArmed <- as.factor(train$SubjectArmed)
train$SubjectRace <- as.factor(train$SubjectRace)
train$SubjectGender <- as.factor(train$SubjectGender)
train$NatureOfStop <- as.factor(train$NatureOfStop)
train$Department <- as.factor(train$Department)
train$FullNarrative <- as.factor(train$FullNarrative)
train$Notes <- as.factor(train$Notes)



test$OfficerRace <- as.numeric(test$OfficerRace)
test$OfficerGender <- as.numeric(test$OfficerGender)

test$City <- as.factor(test$City)
test$SubjectArmed <- as.factor(test$SubjectArmed)
test$SubjectRace <- as.factor(test$SubjectRace)
test$SubjectGender <- as.factor(test$SubjectGender)
test$NatureOfStop <- as.factor(test$NatureOfStop)
test$Department <- as.factor(test$Department)
test$FullNarrative <- as.factor(test$FullNarrative)
test$Notes <- as.factor(test$Notes)


test$SubjectArmed<-factor(test$SubjectArmed, levels=levels(train$SubjectArmed))
test$SubjectRace<-factor(test$SubjectRace, levels=levels(train$SubjectRace))
test$SubjectGender<-factor(test$SubjectGender, levels=levels(train$SubjectGender))
test$NatureOfStop<-factor(test$NatureOfStop, levels=levels(train$NatureOfStop))
test$FullNarrative<-factor(test$FullNarrative, levels=levels(train$FullNarrative))
test$Notes<-factor(test$Notes, levels=levels(train$Notes))



#' 
#' 
#' 
#' 
#' #Subsetting data
#' 
## ------------------------------------------------------------------------

train1 <- train %>% select(-City, -id, -Department)
test1 <- test %>% select(-City, -id, -Department)



#' 
#' 
#' #Figuring out some subsets
## ------------------------------------------------------------------------
#Full Narrative no go

#Notes
#Levels: accident hit miss None other weapon

train2 <- train[train$Notes == "hit",]
table(train2$Fatal)




#' 
#' 
#' # Predicting
#' 
## ------------------------------------------------------------------------

# us = which(train1$Fatal == "U")
# set.seed(123)
# i = sample(1:189, 63)
# train1$Fatal[us[i]] = "F"
# train1$Fatal[us[-i]] = "N"

# logistic = glm(factor(Fatal) ~ NumberOfShots + SubjectArmed + SubjectGender + SubjectRace + SubjectAge + NatureOfStop + NumberOfShots + NumberOfOfficers + OfficerGender + OfficerRace + BlackProp + MedianIncome +
#                ViolentCrime + LawEnforcementOfficersProp, 
#              data = train1, family = binomial)
# pred = predict(logistic, type = "response")
# pred.fatal = pred < 0.5


full = tree(Fatal ~ SubjectArmed + SubjectGender + SubjectRace + SubjectAge + NatureOfStop +
              NumberOfShots + NumberOfOfficers + OfficerGender + OfficerRace + BlackProp + 
              MedianIncome + ViolentCrime + LawEnforcementOfficersProp + FullNarrative +
              News,
            data = train1, minsize = 2)
plot(full)
text(full, pretty = TRUE, cex = 0.7)

tree_pred = as.vector(predict(full, newdata = test1, type = "class"))

# pred[which(train$News == 1)] <- "F"


# pred = as.vector(predict(full, newdata = test1, type = "class"))
# out = as.data.frame(cbind(test$id, pred))
# colnames(out) = c("id","Fatal")
# 
# out$Fatal = as.character(out$Fatal)
# out[out$Fatal == "F",]$Fatal = "Yes"
# out[out$Fatal == "N",]$Fatal = "No"
# 
# write.csv(out, file = "6_4response.csv", row.names = FALSE)


#' 
#' #Random Forest
#' 
## ------------------------------------------------------------------------
set.seed(123)
tuneRF(train1[,-3], train1$Fatal) #mtry = 2

forest1 <- randomForest(Fatal ~ ., 
              data = train1, mtry = 2, importance = TRUE)

forest1

forest2 <- randomForest(Fatal ~ ., 
              data = train1, mtry = 4, cutoff = c(0.45, 0.55))

forest2


# pred = as.vector(predict(forest2, newdata = train1, type = "class"))
# table(pred, train$Fatal)
# mean(pred!=train$Fatal)


pred = as.vector(predict(forest2, newdata = test1, type = "class"))
out = as.data.frame(cbind(test$id, pred))
colnames(out) = c("id","Fatal")

out$Fatal = as.character(out$Fatal)
out[out$Fatal == "F",]$Fatal = "Yes"
out[out$Fatal == "N",]$Fatal = "No"

write.csv(out, file = "6_5_forest_response.csv", row.names = FALSE)


#' 
#' 
#' # Boosting
#' 
## ------------------------------------------------------------------------

library(caret)
library(xgboost)
library(e1071)

#
myparamGrid <- expand.grid(colsample_bytree=c(0.1,0.5,0.9),
                            max_depth = c(1,2,3,4,5)^2 ,
                           eta = seq(from=0.1, to=1, by=0.1),
                           nrounds=c(600,300,100),
                           gamma=1,
                           min_child_weight= seq(from = 1, to=10, by = 3),
                           subsample = 1)
##

# training set is stored in sparse matrix: devmat
myparamGrid <- expand.grid(colsample_bytree=c(0.1,0.5, 0.9),
                           max_depth = c(1,10,20,30) ,
                          eta = seq(from=0.1, to=1, by=0.1),
                          nrounds=c(600,300,100),
                          gamma=1,
                          min_child_weight= c(1,2,3),
                          subsample = 1)

fitControl <- trainControl(
                           method = "cv"
                           , number = 2  # 2-fold CV
                           #, repeats = 5 # repeated 5 times
                           , verboseIter=TRUE
                           ,savePredictions = TRUE
                           ,classProbs = TRUE
                           , returnData=FALSE )

tuneFit <- train( Fatal ~., data = train1
        , method = "xgbTree"
        , metric = "Kappa" # "Accuracy" #
        , trControl = fitControl
        , tuneGrid = myparamGrid
        )

print(tuneFit)
best_Model <- tuneFit$finalModel

pred = as.vector(predict(tuneFit, newdata = test1))

out = as.data.frame(cbind(test$id, pred))
colnames(out) = c("id","Fatal")

out$Fatal = as.character(out$Fatal)
out[out$Fatal == "F",]$Fatal = "Yes"
out[out$Fatal == "N",]$Fatal = "No"

write.csv(out, file = "6_9_xgboost_response.csv", row.names = FALSE)
    
#########################################




#' 
#' 
#' # Graph
#' 
## ------------------------------------------------------------------------
forest_error <- 1 - 0.79285
boost_error <- 1 - 0.76190
tree_error <- 1 - 0.74761

forest_private_error <- 1 - 0.74489
boost_private_error <- 1 - 0.73673
tree_private_error <- 1 - 0.73673
  
errors <- matrix(c(forest_error, forest_private_error, boost_error, boost_private_error, 
            tree_error, tree_private_error), nrow = 2)

barplot(errors, beside = TRUE, col = c("gold", "light blue"),
        names.arg = c("Random Forest", "XgBoost", "Tree"),
        xlab = "Model Method",
        ylab = "Error Rate",
        main = "Testing Error Rates by Method",
        ylim = c(0,0.4),
        legend.text = c("Public", "Private"))


#'               
#'               
