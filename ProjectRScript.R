##convert NA values to 0's
ProjectR[is.na(ProjectR)] <- 0

## Attach Data
attach(ProjectR)

## Summary Statistics of all Vars
summary(ProjectR)

## Histogram of Q3 to display technology adoption
hist(ProjectR$Q3_1, 
    main="On a scale of 0-100, how soon do you adopt technology?",
    xlim=c(0,100),
    xlab="Response",
    breaks=c(0,10,20,30,40,50,60,70,80,90,100),
    col=c("black","purple","blue","turquoise","turquoise","green","yellow","orange","orangered","red")
    )

## Pie graph to show adoption bins
slices <- c(2,3,4,5)
lbls <- c("Late Majority","Early Majority","Early Adopter","Innovator")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Technology Adoption Segmentation",
    radius = 1)

## Load Plotly into library
library(plotly)

## Bar graph to show technology awareness
Q51 <- sum(Q5_1)
Q52 <- sum(Q5_2)
Q53 <- sum(Q5_3)
Q54 <- sum(Q5_4)
Q55 <- sum(Q5_5)
Q56 <- 0 #None chose the "none" response so hardcoding to 0 due to the var being returned as a character
a <- plot_ly(x = c(Q51, Q52, Q53, Q54, Q55, Q56), 
        y = c('Amazon Alexa', 'Google Asst.', 'Msft. Cortana','Apple Siri','Other','None'),
        type = 'bar', 
        orientation ='h'
        )
layout(a, margin = (l = 275))

## Bar graph to show technology use
Q61 <- sum(Q6_1)
Q62 <- sum(Q6_2)
Q63 <- sum(Q6_3)
Q64 <- sum(Q6_4)
Q65 <- sum(Q6_5)
Q66 <- sum(Q6_6)
b <- plot_ly(x = c(Q61, Q62, Q63, Q64, Q65, Q66), 
             y = c('Amazon Alexa', 'Google Asst.', 'Msft. Cortana','Apple Siri','Other','None'),
             type = 'bar', 
             orientation ='h'
)
layout(b, margin = (l = 200))

## Pie chart to show how often consumers used technology

slices2 <- c(1,2,3,4,5)
lbls2 <- c("0","1-4","5-9","10-14","15+")
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2)
lbls2 <- paste(lbls2, "%",sep="")
pie(slices2,labels = lbls2, col=rainbow(length(lbls)),
    main="# Times Voice Tech Used in Last Month",
    radius = 1)

## Coding Q9 to make a bar chart
Q91 <- length(which(ProjectR$Q9 == 1))
Q92 <- length(which(ProjectR$Q9 == 2))
Q93 <- length(which(ProjectR$Q9 == 3))

# Bar Chart showing Consumer willingness to use voice technology to manage credit card account
c <- plot_ly(y = c(Q91, Q92, Q93), 
             x = c('Yes', 'Maybe', 'No'),
             type = 'bar'
)
layout(c, margin = (l = 275))

## Bar graph to show why not interested in voice technology
Q101 <- sum(Q10_1)
Q102 <- sum(Q10_2)
Q103 <- sum(Q10_3)
Q104 <- sum(Q10_4)
Q105 <- sum(Q10_5)
Q106 <- sum(Q10_6)
d <- plot_ly(y = c(Q101, Q102, Q103, Q104, Q105, Q106), 
             x = c('Does not benefit me', 'Not accurate', '3rd party private info','Fraud Concern','3rd party tracking','Other'),
             type = 'bar'
)
layout(d, margin = (l = 200))

################################################################ investigate correlations
## Mounting scrubbed data with all text boxes removed
library(readr)
ProjectRScrubbed <- read_csv("C:/Users/pryor/OneDrive/Fisher MBA/BUSML 7201 - Market Research/Project/ProjectRScrubbed.csv")
##convert NA values to 0's
ProjectRScrubbed[is.na(ProjectRScrubbed)] <- 0

## Attach Data
attach(ProjectRScrubbed)

## Convert all values of scrubbed file to numeric
ProjectRScrubbed$Q5_6 <- as.numeric(ProjectRScrubbed$Q5_6)
ProjectRScrubbed$Q11_6 <- as.numeric(ProjectRScrubbed$Q11_6)

## Global correlation across scrubbed file
c <- cor(ProjectRScrubbed)
corrplot(c)

## Kitchen Sink linear modeling to investigate using likelihood to use voice command management of credit accounts as dependent variable
L1 <- lm(ProjectRScrubbed$Q9~ . , data=ProjectRScrubbed)
stargazer(L1, type="text")
summary(L1)

### Q5_6 and Q11_6 can be removed as they have no data
ProjectRScrubbed <- subset(ProjectRScrubbed, select = -Q5_6)
ProjectRScrubbed <- subset(ProjectRScrubbed, select = -Q11_6)

## Rerun Kitchen Sink Model
L1 <- lm(ProjectRScrubbed$Q9~ . , data=ProjectRScrubbed)
summary(L1)

## It appears questions 11 and 14 have the most influence in determining the likelihood of voice command management use
### We will now simplify the model to only use questions 11 and 14
L2 <- lm(ProjectRScrubbed$Q9~ Q14 + Q11_1 + Q11_2 + Q11_3 + Q11_4 + Q11_5, data=ProjectRScrubbed)
summary(L2)