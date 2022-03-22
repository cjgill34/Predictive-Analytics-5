#Question 1
install.packages("ROSE")
library(ROSE)
data(hacide)
str(hacide.train)
plot(hacide.train$cls)
hist(hacide.train$x1)
hist(hacide.train$x2)
table(hacide.train$cls)
sum(hacide.train$cls==1)/length(hacide.train$cls)

#Question 2
library(rpart) 
library(party) 
library(partykit)
D1= rpart(cls ~ ., data = hacide.train)
plot(D1)
text(D1)
plot(as.party(D1))
printcp(D1)
plotcp(D1)

D1.pred <- predict(D1, hacide.test, decision.values=TRUE, type="class")
D1.CM=table(D1.pred = D1.pred, true = hacide.test$cls)
D1.CM
sum(diag(D1.CM))/sum(sum(D1.CM))
D1.CM[2, 2]/(D1.CM[2, 2] + D1.CM[1, 2])
D1.CM[2, 2]/(D1.CM[2, 2] + D1.CM[2, 1])
(2*D1.CM[2, 2])/((2*D1.CM[2, 2]) + D1.CM[2, 1]+D1.CM[1, 2])

#Question 3
over.hacide <- ovun.sample(cls ~ ., data = hacide.train, method = "over")$data
table(over.hacide$cls)
summary(over.hacide)


#Question 4
D2= rpart(cls ~ ., data = over.hacide)
plot(D2)
text(D2)
plot(as.party(D2))
printcp(D2)
plotcp(D2)

D2.pred <- predict(D2, hacide.test, decision.values=TRUE, type="class")
D2.CM=table(D2.pred = D2.pred, true = hacide.test$cls)
D2.CM
sum(diag(D2.CM))/sum(sum(D2.CM))
D2.CM[2, 2]/(D2.CM[2, 2] + D2.CM[1, 2])
D2.CM[2, 2]/(D2.CM[2, 2] + D2.CM[2, 1])
(2*D2.CM[2, 2])/((2*D2.CM[2, 2]) + D2.CM[2, 1]+D2.CM[1, 2])

#Question 5
library(pROC)
par(pty="s")
D1test_prob = as.numeric(predict(D1, hacide.test, decision.values=TRUE,type = "class"))-1
D1test_roc = roc(hacide.test$cls ~ D1test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
auc(D1test_roc)
par(pty="m")

par(pty="s")
D2test_prob = as.numeric(predict(D2, hacide.test, decision.values=TRUE,type = "class"))-1
D2test_roc = roc(hacide.test$cls ~ D2test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
auc(D2test_roc)
par(pty="m")
