#+ code-a, echo=FALSE
library(plyr)
library(ggplot2)
recategorization = read.csv("C:\\Users\\LukasHalim\\Documents\\Dropbox\\recategorization.csv",stringsAsFactors=FALSE)
transactions = read.csv(file.choose(),stringsAsFactors=FALSE)

transactions$Date <- as.Date(transactions$Date , "%m/%d/%Y")
current_week <- subset(transactions, Date > Sys.Date() - 7 
                        & Transaction.Type == "debit" 
                        & Category != "Charity")
current_week2 <- join(current_week,recategorization,type="left")
current_week3 <- current_week2
current_week3$New_Category[is.na(current_week3$New_Category)] <- current_week2$Category[is.na(current_week2$New_Category)]

cdata <- ddply(current_week3, .(New_Category), summarize, Amt=sum(Amount))
cdata2 <- cdata[with(cdata, order(-Amt)), ]

cdata2$New_Category <- factor(cdata2$New_Category, levels=cdata2$New_Category)
cdata2$New_Category <- reorder(cdata2$New_Category, -cdata2$Amt)

#'Plot of Prior Week Spending
ggplot(data=cdata2, aes(x=New_Category, y=Amt)) + 
  geom_bar(stat="identity") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
  

#'Total Prior Week Spending
Total <- ddply(current_week, .(), summarize, Amt=sum(Amount))
Total$Amt

#'Table of Prior Week Spending
current_week3[,c("Date","Description","New_Category","Amount")]

