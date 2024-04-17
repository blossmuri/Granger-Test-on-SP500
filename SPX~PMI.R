# 以每五年為一個跨度，from 1983 - 2003
# 嘗試利用ccf & granger's causality test，判斷PMI作為SPX領先指標是否有統計上顯著性 

#我把滯後期數改成用ccf得出的lag_time
#原本用var.select得出來的感覺像是optimized的結果，雖然會有不錯的p value，但看經濟上的解釋沒有道理。因此granger test 跟 OLS  都改用ccf的lag_time來回歸。

install.packages("lmtest") ; install.packages("vars") ; install.packages("readxl") ; install.packages("car") ; install.packages("ppcor")
library(lmtest) ; library(ggplot2) ; library(vars) ; library(readxl) ; library(car) ; library(ppcor)

PMI <- read_excel("data/PMI.xlsx")
SPX <- read_excel("data/SPX.xlsx")
options(digits = 5)
Table <- list()

############################ Each 5-years


for (i in c(1,2,3,4,5,6,7,8)){  j <- paste((2023-5*i),"~",(2023-5*(i-1)))

  Table[[j]] <- list() 
  
  for (l in 1:4){
    Table[[j]][l] <- list()
  }
  
  SPXplot <- ts(rev(SPX$YoY[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)
  PMIplot <- ts(rev(PMI$YoY[(1 + 60*(i - 1)) : (60*i)]), start = c((2023-5*i), 3), frequency = 12)
  
  plot(SPXplot, main = "SPX and PMI Series", ylim= c(-25, 40), xlab = "Time", ylab = "% Change") ; lines(PMIplot, col = "blue")
  
  plot(SPX$dYoY[(1 + 60*(i - 1)) : (60*i)],PMI$dYoY[(1 + 60*(i - 1)) : (60*i)],
       xlab = "SPX", ylab = "PMI", main = paste((2023-5*i),"~", (2023-5*(i-1) )))
  

  # test 1. Cross-Correlation analysis
  
  PMIseries <- as.vector(as.numeric(PMI$dYoY[(1 + 60*(i - 1)) : (60*i)]))
  SPXseries <- as.vector(as.numeric(SPX$dYoY[(1 + 60*(i - 1)) : (60*i)]))
  
  ccf_result <- ccf(PMIseries, SPXseries, lag.max = 6, plot = FALSE)
  
  max_corr <- max(ccf_result$acf)   
  lag_index <- which(ccf_result$acf == max_corr)  
  lag_time <- as.numeric(ccf_result$lag[lag_index])   
  
  Table[[j]][1] <- list(c(max_corr,lag_time))
  
  
  # test 2. Granger's causality test
  
  if (lag_time >= 0) {
    
     PMIseries <- as.vector(as.numeric(PMI$dYoY[(1 + 60*(i - 1)) : (60*i)]))
     SPXseries <- as.vector(as.numeric(SPX$dYoY[(2 + 60*(i - 1)) : (60*i+1)]))
    
     dat <- data.frame(Y = SPXseries , X = PMIseries)

     G <- list(grangertest( Y ~ X , order = (lag_time+1) , data = dat))
    
  
  } else { 
    
     G <- "Not applicable"
     
     
  }
  
  Table[[j]][2] <- G
  
  
  # test 3. partial correlation
  
  if (lag_time >= 0) {
    
    
    y = as.vector(as.numeric(SPX$dYoY[(1 + 60*(i - 1)) : (60*i)]))
    
    SPXsub <- matrix(0 , nrow = 60, ncol = (lag_time + 1) )
    PMIsub <- matrix(0 , nrow = 60, ncol = (lag_time + 1) )
    
    for (k in 1:60) {
      
      PMIsubset <- as.vector(as.numeric(PMI$dYoY[( 60*(i - 1) + k) : (60*(i-1) + k + (lag_time))]))
      SPXsubset <- as.vector(as.numeric(SPX$dYoY[( 1+60*(i - 1) + k) : (1 + 60*(i-1) + k  + (lag_time))]))
      
      SPXsub[k, ] <-  SPXsubset
      PMIsub[k, ] <-  PMIsubset
      
      
    }
    
    PC <-data.frame( y , cbind(SPXsub,PMIsub))
    CO <- cor(PC)
    PCO <- pcor(CO)
    
  } else {
  
    PCO <- "Not applicable"
  }
  
  Table[[j]][3] <- PCO
}


############################ Result


Table
SPXplot <- ts(rev(SPX$dYoY[1:180]), start = c(2008, 4), frequency = 12)
PMIplot <- ts(rev(PMI$dYoY[1:180]), start = c(2008, 4), frequency = 12)
plot(SPXplot, main = "SPX and PMI Series",ylim= c(-45, 60), xlab = "Time", ylab = "Value") ; lines(PMIplot, col = "blue")

