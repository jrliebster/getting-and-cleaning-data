set.seed(1)
rpois(5, 2)

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

x + y + e

library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

system.time()

#load packages
library(pacman)
p_load(readr, dplyr, janitor, tidyr)

# read outcome file

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)

# make a histogram of 30-day death rates by heart attack
# need to first make column class numberic, since cols were read in as character
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings


best<- function(state, outcome)
{
    outcome1 <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    if(!any(state == outcome1$State)){
        stop("invalid state")}
    else if((outcome %in% c("heart attack", "heart failure",
                            "pneumonia")) == FALSE) {
        stop(print("invalid outcome"))
    }
    outcome2 <- subset(outcome1, State == state)
    if (outcome == "heart attack") {
        colnum <- 11
    }
    else if (outcome == "heart failure") {
        colnum <- 17
    }
    else {
        colnum <- 23
    }
    min_row <- which(as.numeric(outcome2[ ,colnum]) == 
                         min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
    hospitals <- outcome2[min_row,2]
    hospitals <- sort(hospitals)
    return(hospitals[1])
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

# top <- group_by(outcome, "heart attack", State) %>% 
   # filter("heart attack" == min("heart attack")) %>% 
   # filter(1:n() == 1)

## Return hospital name in that state with lowest 30-day death rate
#rank hospital by 'outcome':

rankhospital <- function(state, outcome, num = "best") {
    
    # Read outcome data
    out_dt <- data.table::fread('outcome-of-care-measures.csv')
    
    outcome <- tolower(outcome)
    
    # Column name is same as variable so changing it 
    chosen_state <- state 
    
    # Check that state and outcome are valid
    if (!chosen_state %in% unique(out_dt[["State"]])) {
        stop('invalid state')
    }
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome')
    }
    
    # Renaming Columns to be less verbose and lowercase
    setnames(out_dt
             , tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
    )
    
    #Filter by state
    out_dt <- out_dt[state == chosen_state]
    
    # Columns indices to keep
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
    
    # Filtering out unnessecary data 
    out_dt <- out_dt[, .SD ,.SDcols = col_indices]
    
    # Find out what class each column is 
    # sapply(out_dt,class)
    out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
    
    # Removing Missing Values for numerical datatype (outcome column)
    out_dt <- out_dt[complete.cases(out_dt),]
    
    # Order Column to Top 
    out_dt <- out_dt[order(get(outcome), `hospital name`)]
    
    out_dt <- out_dt[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
    
    if (num == "best"){
        return(out_dt[1,`hospital name`])
    }
    
    if (num == "worst"){
        return(out_dt[.N,`hospital name`])
    }
    
    return(out_dt[num,`hospital name`])
    
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

rankall <- function(outcome, num = "best") {
    
    # Read outcome data
    out_dt <- data.table::fread('outcome-of-care-measures.csv')
    
    outcome <- tolower(outcome)
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome')
    }
    
    # Renaming Columns to be less verbose and lowercase
    setnames(out_dt
             , tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
    )
    
    # Columns indices to keep
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
    
    # Filtering out unnessecary data 
    out_dt <- out_dt[, .SD ,.SDcols = col_indices]
    
    # Find out what class each column is 
    # sapply(out_dt,class)
    
    # Change outcome column class
    out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
    
    if (num == "best"){
        return(out_dt[order(state, get(outcome), `hospital name`)
                      , .(hospital = head(`hospital name`, 1))
                      , by = state])
    }
    
    if (num == "worst"){
        return(out_dt[order(get(outcome), `hospital name`)
                      , .(hospital = tail(`hospital name`, 1))
                      , by = state])
    }
    
    return(out_dt[order(state, get(outcome), `hospital name`)
                  , head(.SD,num)
                  , by = state, .SDcols = c("hospital name") ])
    
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

