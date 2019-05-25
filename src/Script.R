#
# Author: Piyush Jhamb
# Script Use Cases: This assignment model script is helpful in solving problems which has
#                   applications in education, marketing, retail and HR analytics domain. 
#                   This script evaluates first the matrix should be a n * n matrix and then
#                   solve it.

# Steps:
# convert excel file to csv
# Read it by removing first row
# transpose dataframe
# Check rows and column of the dataframe
# Confirm it as n*n dataframe
# Convert it to Matrix
# Operate using assignment model, keep intermediate solutions
# now Multiply intermediate solution to matrix, convert it to dataframe
# transpose it back
# Add first row 
# write a new csv file with emp name and assigned job role to it
# Job role will define the job family


# Loading the required libraries:

#install.packages(pacman)
#library(pacman)
#p_load(dplyr, magrittr, lpSolve, matrixcalc, readr, tibble, readxl, here)

library(tidyverse)
library(dplyr)
library(magrittr)
library(lpSolve)
library(matrixcalc)
library(readr)
library(readxl)
library(tibble)
library(here)
library(data.table)

# Reading a file

#print("Can you please share path of the excel file, for example like : C:\Users\piyush.jhamb")
system.time(rating <- read_csv(file=file.choose(), col_names = TRUE))
#rating <- read_csv(here("data", "SampleBookOfExp.csv"), col_names = TRUE)

# Global Filename variable:
filename <- here("Output", paste0("Solution", format(Sys.Date(), "%d-%m-%Y"), ".csv"))

# Transpose the dataframe
#ratingT <- t(rating)

# validate file structure
str(rating)
dim(rating)

#cut first row 

#rating <- t(rating)


#cut the first column in the process
# Keep list of column one in List
nameList<-rating[,"Name"]
rating_withoutJob <- select(rating, -starts_with("Name"))
colName <- colnames(rating_withoutJob)

# getting Number of rows
num_rows <- nrow(rating_withoutJob)
num_rows

if(nrow(rating_withoutJob)==ncol(rating_withoutJob)){
  #Convert to matrix for assignment model
  ratings_matrix <- data.matrix(rating_withoutJob)
  # Create assignment Model for evaluation:
  abc <- lp.assign (ratings_matrix, direction = "max")
  # Check status of the model : Correct if model outout is 0, otherwise didn't converge
  abc$status
  
  #if(abc$status!=0){
  #  print("model did not converge, some error is there")
  #}else{
  
  # Check solution of the assignment model, 1 = Assignment done, 0= no assignment
  testSolutions <- abc$solution
  
  ## Mutiply matrix solution
  ratings_matrix_Final <- ratings_matrix*testSolutions
  
  # Convert it to dataframe again
  
  SolutionDF <- as.data.frame(ratings_matrix_Final)
  Solution1 <- as.data.frame(cbind(nameList,SolutionDF))
  
  
  
  
  ## Add First column again to the solution and write to csv
  #write.csv(t(Solution1), file="Solution.csv", na = "", row.names = FALSE)
  # rownames_to_column(Solution1)
  TransposeDataSet <- t(Solution1)
  
  
 
  
  
  for(i in 2:nrow(TransposeDataSet)){
    for(j in 1:ncol(TransposeDataSet)){
      if(TransposeDataSet[[i,j]]!=" 0"){
        #print("got non zero values")
        #outputFile <- paste(c(rownames(abc)[i], abc[[1,j]]), sep = ",")
        #print(abc[[1,j]])
        df <- cbind(rownames(TransposeDataSet)[i], TransposeDataSet[[1,j]])
        
        write.table(df, file = filename, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE) 
        #col.names = c('Emp','job')
      }
      
    }
    
  }

  
  
  
  
  #remove all .data dataframes
  #rm(list=ls(all=TRUE))
  
}else{
  print("File is not a n*n Matrix, means number of rows does not equal to columns")
  break
}



# Pacman dependency addition to load library
# Use of Custom functions
# use of Shiny server for app like interface, bit lengthy process


