library("ggplot2")
data("diamonds")

#Q1：
methods(class=data.frame) #Print to the console all methods
attributes(diamonds) #Print all attributes
ncol(diamonds) #count the number of columns in a dataframe “diamonds”

#Q2:
nrow(diamonds) #count how many rows are in a dataframe

#Q3:
colna <- function(data){
#this function extracts the column names from a dataframe and print the names
#of the columns (one per line) to the console
writeLines(colnames(data)) 
}
colna(diamonds)

#Q4:
type <- function(data){
#this function determines the type of each column in a dataframe
sapply(data,class)
}
type(diamonds)

#Q5:
mean_data <- function(data){
#this function loops through any dataframe and calculate the mean of every numeric column.
#Label the output with the name of the column.
sapply(data[sapply(data,is.numeric)],mean,na.rm=TRUE)
}
mean_data(diamonds)

#Q6:
frequency_data <- function(data){
#this function loop through any dataframe and create a frequency table for every factor
#column. Label the output with the name of the column
lapply(Filter(is.factor,data),table) 
}
frequency_data(diamonds)

#Q7:
freq_na <- function(data){
  #this function will loop through any dataframe and determine the number of rows 
  #containing NA (missing	value) in each column and the percentage of rows containing
  #an NA in any of the columns
  counts <- apply(sapply(data,is.na),2,sum) # count the number of NA in each column
  percent <- sapply(data, function(x) sum(is.na(x))/length(x))# count the percentage of NA in each column
  df <- rbind(counts, percent)
  return(df)
}
freq_na(diamonds)

#Q8:
Pearson_coeff <- function(dafra) 
{
  #this function, Pearson_coeff(),can accept any dataframe as a parameter and
  #returns a dataframe that contains each pair of column names in the first column 
  #in a single string separated by a -
  
  #parameter: 
  #dafra- a dataframe been input
  
  #return:
  #a dataframe that contains each pair of column names in the first column 
  #in a single string separated by a -
  dafra <- Filter(is.numeric,dafra) # take out all numeric columns
  colna <- colnames(dafra) #extract the column names
  pairwise_names=c()#create a variable who represents pairwise varialbes' name
  pairwise_cor=c()#create a variable who represents pairwise varialbes' Pearson correlation coefficient
  for(i in 1:(length(colna)-1))# loop the variables from first to the 2nd last 
  {
    for(j in (i+1):length(colna)) #loop the variables from (i+1)th to the end, in case of the repeat
    {
      temp <- cor(dafra[i],dafra[j],method="pearson") #caculate the pairwise Pearson correlation coefficient
      pairwise_names <- c(pairwise_names,paste(colna[i],colna[j],sep="-")) # add pairwise variables' names one by one
      pairwise_cor <- c(pairwise_cor,temp)#add pairwise variables' Pearson correlation coefficients correspondingly
    }
   }    
   return (data.frame(pairwise_names,pairwise_cor)) #return a new dataframe
}
Pearson_coeff(diamonds) #check the "diamonds"

#Q9:
Scatterplot <- function(dafra)
{
  # this function,Scatterplot(), creates and label a scatter plot for every pair of numeric variables. Add a title
  #to the plot that contains the combined name of the pair from problem 8 and the calculated Pearson correlation coefficient of the pair.
  
  #parameter: dafra- a data frame been taken
  
  #return: NULL but draw scatterplot of pairwise numeric data
  library(ggplot2) # call the ggplot
  dafra <- Filter(is.numeric,dafra) # take out all numeric data
  colna <- colnames(dafra) #get the every column's name
  combos <- combn(colna, 2) #combine 2 columns' names without repeat
  P_CF <- Pearson_coeff(dafra)# Call the function Pearson_coeff() in Question 8
  for (i in 1:nrow(P_CF)){ # draw the scatter plots one by one
    p <- ggplot(dafra, aes(x = dafra[,combos[1,i]], y = dafra[,combos[2,i]])) + #according to combn()'s result, draw corresponding scatterplots
      xlab(combos[1,i])+ylab(combos[2,i])+ #change its labels
      geom_point(size = 0.3) + #change its plot size
      ggtitle(paste(P_CF[i,1], P_CF[i,2], sep = '  r = ')) # give it a title
    print(p) #draw this plot
  }
}
Scatterplot(diamonds) #check "diamonds",remember to run Funtion Pearson_coeff() simultaneously
  

  