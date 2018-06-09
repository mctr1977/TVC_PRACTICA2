summary(Titanic)
str(Titanic)

names(Titanic)
Titanic <- within(Titanic, {
  PassengerId <- NULL 
})
Titanic <- within(Titanic, {
  Ticket <- NULL 
})
Titanic <- within(Titanic, {
  Name <- NULL 
})
Titanic <- within(Titanic, {
  Embarked <- NULL 
})
sapply(Titanic, function(x)(sum(is.na(x)))) 
Titanic <- within(Titanic, {
  SibSp <- NULL 
})
Titanic <- within(Titanic, {
  Parch <- NULL 
})
sapply(Titanic, function(x)(sum(is.na(x))))
summary(Titanic)
str(Titanic)
Titanic <- within(Titanic, {
  Fare <- NULL 
})
sapply(Titanic, function(x)(sum(is.na(x))))
summary(Titanic)
str(Titanic)

Titanic$Age[is.na(Titanic$Age)] <- 28

Titanic <- within(Titanic, {
  Cabin <- NULL 
})
str(Titanic)
Titanic <- within(Titanic, {
  Survived <- as.factor(Survived)
})
Titanic <- within(Titanic, {
  Pclass <- as.factor(Pclass)
})
Titanic <- local({
  .Z <- scale(Titanic[,c("Age")])
  within(Titanic, {
    Z.Age <- .Z[,1] 
  })
})
 Titanic$Age<- ifelse(Titanic$Age<=16,1,Titanic$Age)
Titanic$Age<- ifelse(Titanic$Age<=60 & Titanic$Age>1 ,2,Titanic$Age)
Titanic$Age<- ifelse(Titanic$Age>2,3,Titanic$Age)
Titanic <- within(Titanic, {
  Pclass <- as.factor(Pclass)
})
Titanic <- within(Titanic, {
  Survived <- as.factor(Survived)
})
Titanic <- within(Titanic, {
  Age <- as.factor(Age)
})
local({
  .Table <- xtabs(~ Survived , data= Titanic )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})
local({
  .Table <- xtabs(~ Sex , data= Titanic )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})
local({
  .Table <- xtabs(~ Survived , data= Titanic )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})
with(Titanic, Barplot(Survived, by=Age, style="divided", legend.pos="above", xlab="Survived", ylab="Frequency"))
with(Titanic, Barplot(Survived, by=Pclass, style="divided", legend.pos="above", xlab="Survived", ylab="Frequency"))
with(Titanic, Barplot(Survived, by=Sex, style="divided", legend.pos="above", xlab="Survived", ylab="Frequency"))

