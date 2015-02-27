#Function Best for Assignment 3

best<- function(state, outcome){
  package<- read.csv("~/Desktop/data/outcome-of-care-measures.csv", colClasses="character")
  #Step1: Read the file and assign name
  #Step2: Test validity of state entry and outcome entries
  if(!any(state==package$State)){
      stop("Invalid State")}
  if(outcome=="heart attack"){
      col_id<- 11} else {
        if(outcome=="heart failure"){
          col_id<- 17} else{
            if(outcome=="pneumonia"){
              col_id<- 23} else{
              stop("Invalid Outcome")}}}
  #Step3: Subset the required information and identify the row index for minimimum
  table<- subset(package, package[,col_id] & package$State==state)
  #Step4: Order the results by hospital name
  order_table<- table[order(table[,1])]
  #Step5: Return the minimum rate
  results_row<- which(order_table[,col_id] == min(as.numeric(order_table[,col_id])))
  results<- order_table[results_row, 1]
  print(results)

Compare with:
best <- function(state, outcome) {
  
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  # print(i)
  
  # todo: handle the ties
  data.state <- data[data$State == state, ]
  data.state[, i] <- as.numeric(x=data.state[, i])
  
  data.state <- data.state[complete.cases(data.state), ]
  
  # print(data.state[, c(1, 2, i)])
  # print(data.state[, i])
  # min(data.state[, i]) -> mm
  # print(mm)
  # print(min(data.state[, i], na.rm=TRUE))
  
  # print(data.state[, i] == min(data.state[, i]))
  
  return.names <- data.state[(data.state[, i] == min(data.state[, i])), ]$Hospital.Name
  
  sort(return.names)[1]
}
  
  
