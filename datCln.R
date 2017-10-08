#### Script description
# Run over the whole script by press Ctrl + Alt + R in Rstudio on Windows.
# You will get a list containing 2 data frame: train, test. 
# NOTE: Please use dat$train to do EDA !


#### Datasets ####
# Loading data: train, test
dat.train <- read.csv('train.csv')
dat.test  <- read.csv('test.csv')
dat <- list(train = dat.train, test = dat.test)



#### functions for data cleansing ####

## Set 4 predictors as factors
# data.frame -> data.frame
# Set 4 predictors as Factor
setFactor <- function(dat) {
    l <- c("NaturallyAspirated", "Supercharged", "Turbocharged")
    dat$TransLockup <- factor(dat$TransLockup)
    dat$VarValveTiming <- factor(dat$VarValveTiming)
    dat$VarValveLift <- factor(dat$VarValveLift)
    dat$AirAspirationMethod <- factor(dat$AirAspirationMethod, 
                                      ordered = TRUE,levels = l)
    dat
}


## 1. Add new predictor: Drive by compressing DriveDesc to 3 categories: 
## FWD, RWD, and 4WD.(ordered)

# data.frame -> data.frame
# Add new predictor: Drive from DriveDesc
addDrive <- function(dat) {
    dat$Drive <- factor(sapply(dat$DriveDesc, mapDrive),
                        ordered = TRUE, levels = c("FWD", "RWD", "4WD"))
    dat
}

# Char -> Char
# Drive mapping function
mapDrive <- function(d) {
    m <- nchar(as.character(d))
    if (substr(d, 1, 1) != "T") {
        return("4WD")
    } else if (substr(d, m, m) == "t") {
        return("FWD")
    } else if (substr(d, m, m) == "r") {
        return("RWD")
    } else {
        "!!!"
    }
}
# # test:
# "4WD" == mapDrive("AllWheelDrive")
# "4WD" == mapDrive("FourWheelDrive")
# "4WD" == mapDrive("AllWheelDrive")
# "FWD" == mapDrive("TwoWheelDriveFront")
# "RWD" == mapDrive("TwoWheelDriveRear")


## 2. Combine 'ValvesPerCylinder': 
# data.frame -> data.frame
# Add new predictor: ValvePerCyl from predictors: IntakeValvePerCyl and
# ExhaustValvesPerCyl by sum
addNumValves <- function(dat) {
    sumValve <- dat$IntakeValvePerCyl + dat$ExhaustValvesPerCyl
    dat$ValvePerCyl <- factor(sumValve)
    dat
}


## 3. Make predictor: carClass by deleting Drvie information from CarLineClass
# data.frame -> data.frame
# Add new predictor: carClass by deleting Drvie information from CarLineClass
addCarClass <- function(dat) {
    carClass <- as.character(dat$CarlineClassDesc)
    isDrive <- regexpr("[2-4]", carClass) > 1
    withDrive <- (carClass[isDrive])
    carClass[isDrive] <- substring(withDrive, 1, nchar(withDrive) - 3)
    dat$carClass <- factor(carClass)
    dat
}


## 4. Add new predictor: Weight from carClass, reference to FEG2017
# !!! We will deal with the order later since category: "Other"

# data.frame -> data.frame
# Add new predictor: weightLevel from carClass
# test:
addWeight <- function(dat) {
    dat$weightLevel <- factor(sapply(dat$carClass, class2weight))
    dat
}

# Char -> Char
# Assign weight levels according to CarLineClass
class2weight <- function(c) {
    if (c == "2Seaters") {
        return ("Lightest")
    } else if (c == "MinicompactCars") {
        return ("Sedan-1")
    } else if (c == "SubcompactCars") {
        return ("Sedan-2")
    } else if (c == "CompactCars") {
        return ("Sedan-3")
    } else if (c == "MidsizeCars") {
        return ("Sedan-4")
    } else if (c == "LargeCars") {
        return ("Sedan-5")
    } else if (c == "SmallStationWagons") {
        return ("Wagon-S")
    } else if (c == "SmallPickupTrucks") {
        return ("Truck-6k")
    } else if (c == "StandardPickupTrucks" ||
               c == "VansCargoTypes" ||
               c == "SpecialPurposeVehicleminivan" ||
               c == "SpecialPurposeVehicleSUV") {
        return ("Truck-8k")
    } else if (c == "VansPassengerType") {
        return ("Truck-10k")
    } else if (c == "Other") {
        return ("Other")
    } else {
        return ("!!!")
    }
}
# # test:
# "Lightest"  == class2weight("2Seaters")
# "Sedan-1"   == class2weight("MinicompactCars")
# "Sedan-2"   == class2weight("SubcompactCars")
# "Sedan-3"   == class2weight("CompactCars")
# "Sedan-4"   == class2weight("MidsizeCars")
# "Sedan-5"   == class2weight("LargeCars")
# "Wagon-S"   == class2weight("SmallStationWagons")
# "Truck-6k"  == class2weight("SmallPickupTrucks")
# "Truck-8k"  == class2weight("StandardPickupTrucks")
# "Truck-8k"  == class2weight("VansCargoTypes")
# "Truck-8k"  == class2weight("SpecialPurposeVehicleminivan")
# "Truck-8k"  == class2weight("SpecialPurposeVehicleSUV")
# "Truck-10k" == class2weight("VansPassengerType")


# a List of Logicals -> Data.frame
# Drop invalid observation by 3 criterion from dat$train
rmObs <- function(lol) {
    wrong <- FALSE 
    for (logic in logicList) wrong <- wrong | logic
    return(dat$train[-which(wrong),])
}


# NULL(dat$train) -> Logicals(list)
# To remove observation from dat$train by incorrect number of wheel drive
selObsByNumDrive <- function() {
    df <- dat$train
    nWDfromCarClass <- gsub("[^0-9]", "", substring(df$CarlineClassDesc, 2))
    nWDfromDrive <- sapply(as.character(df$Drive), 
                  function(c) ifelse(substr(c, 1, 1) == "4", "4", "2"))
    return(list((nWDfromCarClass != nWDfromDrive) & (nWDfromCarClass != "")))
}
# # test:
# sum(unlist(selObsByNumDrive()))


# NULL(dat$train) -> Logicals(list)
# To remove observation from dat$train by incorrect number of gears
selObsByNumGear <- function() {
    df <- dat$train
    numGearFromTrans <- as.numeric(gsub("[^0-9]", "", df$Transmission))
    isAV <- grepl("AV", df$Transmission) # igore AV's num of gear later
    # From NumGears
    numGearFromNumGear <- df$NumGears
    return(list((numGearFromTrans != numGearFromNumGear) & 
               (!isAV & !is.na(numGearFromTrans))))
}
# # test:
# unlist(selObsByNumGear())


# NULL(dat$train) -> Logicals(list)
# To remove observation from dat$train if ValvePerCyl < 2
selObsByValvePerCyl <- function() {
    df <- dat$train
    numVPC <- as.numeric(as.character(df$ValvePerCyl))
    VPCstrange <- numVPC < 2 | numVPC > 4
    return(list(VPCstrange))
}
# # test:
# sum(unlist(selObsByValvePerCyl()))


# data.frame, Chars -> data.frame
# Drop predictors
dropPredictor <- function(df, drops) {
    df[ , - which(names(df) %in% drops)]
}
# # test:
# names(dropPredictor(dat.tmp, c("ID", "FE")))


# Data.frame -> Data.frame
# Correct ValvePerCyl
correctValvePerCyl <- function(df) {
    df$ValvePerCyl <- factor(df$ValvePerCyl)
    return(df)
}



#### Data cleansing steps ####
### Set 4 predictors as factor
dat <- lapply(dat, setFactor)

### Add new predictors:
## 1. Drive
dat <- lapply(dat, addDrive)
## 2. ValvePerCyl
dat <- lapply(dat, addNumValves)
## 3. carClass
dat <- lapply(dat, addCarClass)
## 4. weightLevel
dat <- lapply(dat, addWeight)
# NOTE: predictors: carClass and weightLevel is collinear

### Remove invalid observations from training set
## 1. Complete cases
dat$train <- dat$train[complete.cases(dat$train),]
## 2. Incorrect observations, contradicted by the value of predictors
logicList <- c(selObsByNumDrive(), selObsByNumGear(), selObsByValvePerCyl())
dat$train <- rmObs(logicList)

### Drop useless predictors:
dat <- lapply(dat, dropPredictor, 
              c("ID", "NumGears", "TransCreeperGear", "DriveDesc",
                "IntakeValvePerCyl", "ExhaustValvesPerCyl",
                "CarlineClassDesc"))

### Reduce the level of ValvePerCyl
dat <- lapply(dat, correctValvePerCyl)

### Log FE
dat$train$logFE <- log(dat$train$FE)
dat$train$FE <- NULL


#### Clean environment ####
rm(dat.train, dat.test, logicList)
rm(list = lsf.str())