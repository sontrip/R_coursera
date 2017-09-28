# R_coursera
Completed code from R Programming course Sep17

rankhospital <- function(state,outcome,num="best"){
        
        
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available",stringsAsFactors=FALSE)
        
        ## Keep only the relevant columns and rename them accordingly
        
        if (outcome=="heart attack"){
                
                data2 <- data[,c(2,7,11)]
                names(data2)<-c("hospital","state","heart attack")
        }
        
        else if (outcome=="heart failure"){
                
                data2 <- data[,c(2,7,17)]
                names(data2)<-c("hospital","state","heart failure")
                
        }
        
        else if (outcome=="pneumonia"){
                
                data2 <- data[,c(2,7,23)]
                names(data2)<-c("hospital","state","pneumonia")
        }
        else stop("invalid outcome")
        
        ## Split the data by state and keep only the state we want
        ## Return an error if the state is unspecified
        
        data3<-as.data.frame(split(data2,data2$state)[state])
        
        if (is.null(data3[1,2])==TRUE) stop ("invalid state")
        
        ## Sorts the data frame by the 3rd column using the order function
        
        data4<-data3[order(as.numeric(data3[,3]),data3[,1]),]
        
        ## In the 3rd column, find the last row which has a number before NAs
        worstpos<-as.numeric(length((data4[,3])[!is.na((data4[,3]))]))
        
        
        if (num=="best"){
                data4[1,1]
        }
        
        else if (num=="worst") {
                data4[worstpos,1]
        }
        
        else if (num*1<worstpos+1) { 
                data4[num*1,1]
        }        
        else print(NA)
}
