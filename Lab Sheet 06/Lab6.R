getwd()
setwd("C:\\Users\\Upadya Dhihel\\Desktop\\IT24102123")
getwd()

Delivery_Times <- read.table("Exercise - Lab 05.txt",header = TRUE,sep =',')
Delivery_Times

fix(Delivery_Times)

names(Delivery_Times) <- c("X1")
attach(Delivery_Times)

histogram <- hist(X1,main = "Histograme for Delivery time",
                  breaks = seq(20,70,length = 10),right = FALSE)

breaks <- round(histogram$breaks)
breaks
freq <- histogram$counts
freq
mids <- histogram$mids
mids


classes <- c()

for(i in 1:length(breaks) - 1){
  classes[i] <- paste0("[",break[i],",",breaks[i+1],")")
}

cbind(Classes = classes,frequency = freq)

lines(mids,freq)


cum.freq <- cumsum(freq)
new <- numeric(length(breaks))  # Initialize 'new' as a numeric vector of the same length as 'breaks'

for(i in 1:length(breaks)){
  if(i == 1){
    new[i] = 0
  } else {
    new[i] = cum.freq[i - 1]
  }
}



plot(breaks,new,type = 'l',
     main = "Cumulative Frequency",
     xlab="Delivert Time",
     ylab="Frequency",
     ylim=c(0,max(cum.freq)))




