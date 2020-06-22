# Functions to create
# all of them should result in a hospital name

# 1. best("SC", "heart attack")
# 2. best("NY", "pneumonia")
# 3. best("AK", "pneumonia")
# 4. rankhospital("NC", "heart attack", "worst")
# 5. rankhospital("WA", "heart attack", 7)
# 6. rankhospital("TX", "pneumonia", 10)
# 7. rankhospital("NY", "heart attack", 7)
# 8. r <- rankall("heart attack", 4)
#    as.character(subset(r, state == "HI")$hospital)
# 9. r <- rankall("pneumonia", "worst")
#    as.character(subset(r, state == "NJ")$hospital)
# 10.r <- rankall("heart failure", 10)
#    as.character(subset(r, state == "NV")$hospital)


# 1. Plot the 30-day mortality rates for heart attack

outcomes[, 11] <- as.numeric(outcomes[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcomes[, 11])

#########################################

# 2. Finding the best hospital in a state

# Extract column value based on row index
matr <- matrix(1:9, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C")))
df <- as.data.frame(matr)
df$A[2] # extract the A column value at row index = 2
which.min(df$A)
df$A[which.min(df$B)] # extract the A column value at row index

outcomes$State[which.min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
outcomes$State[which.min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]

which.min(outcomes$Provider.Number)

outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack


#########################################

best <- function(state, outcome) {

  outcomes <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  if (length(which(outcomes$State == state)) == 0) {
    stop("invalid state")
  } else {
    subset_df <- subset(outcomes, State == state)
    cols_to_num <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    subset_df[, cols_to_num] <- lapply(cols_to_num, function(x) as.numeric(as.character(subset_df[[x]])))
  }
    
  if(outcome == "heart attack"){

    subset_df$Hospital.Name[which.min(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
    
  } else if(outcome == "heart failure"){
    
    subset_df$Hospital.Name[which.min(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]
    
  } else if(outcome == "pneumonia"){
    
    subset_df$Hospital.Name[which.min(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
    
  } else {
    stop("invalid outcome")
  }
  
}

# Calling the function
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

# View in a table
View(subset_df)
# View the columns
names(outcomes)

#########################################

# 3. Ranking hospitals by outcome in a state
# the num can either take an integer or the character "best"/"worst" 
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if (length(which(outcomes$State == state)) == 0) {
    stop("invalid state")
  } else {
  ## Return hospital name in that state with the given rank
  subset_df <- subset(outcomes, State == state)
  # Order the subset of the dataframe
  # example: dd[with(dd, order(-z, b)), ] # >> ordering by two columns, z (descending) and b (ascending)
  # Convert to numeric columns
  cols_to_num <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  subset_df[, cols_to_num] <- lapply(cols_to_num, function(x) as.numeric(as.character(subset_df[[x]])))
  }
  
  ## 30-day death rate
  
  if(outcome == "heart attack"){
    subset_df[with(subset_df, order(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Heart.Attack,-xtfrm(subset_df$Hospital.Name))), ]
    subset_df_ordered <- subset_df[with(subset_df, order(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,xtfrm(subset_df$Hospital.Name))), ]
    if(num == "best"){
      # in case best/worst was given as input
      subset_df$Hospital.Name[which.min(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
    } else if(num == "worst"){
      # in case best/worst was given as input
      print(subset_df_ordered$Hospital.Name[which.max(subset_df_ordered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
      print("Worst branch")
    } else {
      # in case the index was given as input
      print(subset_df_ordered[num,"Hospital.Name"])
      #print("INDEX WAS GIVEN")
    }
    
  } else if(outcome == "pneumonia"){
    subset_df_ordered <- subset_df[with(subset_df, order(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,xtfrm(subset_df$Hospital.Name))), ]
    if(num == "best"){
      # in case best/worst was given as input
      print(subset_df_ordered$Hospital.Name[which.min(subset_df_ordered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
      print("Best branch")
    } else if(num == "worst"){
      # in case best/worst was given as input
      print(subset_df_ordered$Hospital.Name[which.max(subset_df_ordered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
      print("Worst branch")
    }
      else {
      # in case the index was given as input
      print(subset_df_ordered[num,"Hospital.Name"])
      #print("INDEX WAS GIVEN")
    }
  }
  else {
    stop("invalid outcome")
  }
}



# INDEPENDENT CHECKS
subset_df <- subset(outcomes, State == "MS")
cols_to_num <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
subset_df[, cols_to_num] <- lapply(cols_to_num, function(x) as.numeric(as.character(subset_df[[x]])))
subset_df_ordered <- subset_df[with(subset_df, order(subset_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,xtfrm(subset_df$Hospital.Name))),]
View(subset_df_ordered)

# MS, SD, WI
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)




r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)



#####################
# RANDOM DATAFRAME
#####################

# ORDERING PRACTICES
# BY MULTIPLE COLUMNS
# FINDINGS:
# 1. HAVE TO CREATE A NEW DATAFRAME TO APPLY THE ORDER FUNCTION
# 2. CHARACTER COLUMN CAN'T BE ORDERED BY PUTTING MINUS SIGN, WE NEED THE xtfrm FUNCTION

employee <- c('John Doe','Peter Gynn','Anne Gynn','Jolie Hope')
salary <- c(25000, 23400, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2018-3-25','2007-3-14'))
employ.data <- data.frame(employee, salary, startdate)
View(employ.data)
employ.data_ordered <- employ.data[with(employ.data, order(employ.data$salary,xtfrm(employ.data$employee))), ]
View(employ.data_ordered)
employ.data_ordered$employee[which.min(employ.data_ordered$salary)]
