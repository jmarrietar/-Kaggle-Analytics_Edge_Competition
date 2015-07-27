setwd("/Users/josemiguelarrieta/Documents/[Kaggle]Analytics_Edge_Competition")
# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

# This script file is intended to help you deal with the text data provided in the competition data files

# If you haven't already, start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

###########
#LOAD DATA#
###########

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
eBayTrain = read.csv("Data/eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("Data/eBayiPadTest.csv", stringsAsFactors=FALSE)

# Now, let's load the "tm" package.

library(tm)

# Then create a corpus from the description variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))

CorpusDescription <- tm_map(CorpusDescription,
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                   mc.cores=1)

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)

# Remember this extra line is needed after running the tolower step:

CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=T)

CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=T)

CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=T)

#CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=T) #ERROR AQUI //Ya no (se arreglo)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusDescription)

sparse = removeSparseTerms(dtm, 0.99)

DescriptionWords = as.data.frame(as.matrix(sparse),row.names = NULL)

# Let's make sure our variable names are okay for R:

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of DescriptionWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(eBayTrain) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTrain"

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))

# The tail function takes the last "n" rows of DescriptionWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(eBayTest) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTest"

DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

# Note that this split of DescriptionWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

#DescriptionWordsTrain$sold = eBayTrain$sold #Not necesary to add anymore. 
DescriptionWordsTrain$WordCount = eBayTrain$WordCount
DescriptionWordsTest$WordCount = eBayTest$WordCount

# Remember that you can always look at the structure of these data frames to understand what we have created


#Approach: Join variables with text variables. 
Data_Train<-cbind(eBayTrain,DescriptionWordsTrain)
Data_Test<-cbind(eBayTest,DescriptionWordsTest)

#Remove description, row.name columns and UniqueID Columns
Data_Train <- Data_Train[ -c(1,11) ]
rownames(Data_Train) <- NULL
Data_Test <- Data_Test[ -c(1,10) ]
rownames(Data_Test) <- NULL

#Apprach: Divide training Data into 2. Train and Test reals. 
library(caTools) 
set.seed(2000)
spl=sample.split(Data_Train$sold,SplitRatio=0.5)
train<-subset(Data_Train,spl==TRUE)
validate<-subset(Data_Train,spl==FALSE)

###################################################################################

#############################################################################
#***APPROACH: Create Logistic Model Just with Description data. 

# Now let's create a logistic regression model using all of the variables:
DescriptionWordsLog = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)

summary(DescriptionWordsLog)
# And make predictions on our test set:

PredTest = predict(DescriptionWordsLog, newdata=DescriptionWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionDescriptionLog.csv", row.names=FALSE)

#############################################################################
# create a logistic regression model using all of the variables:
DescriptionWordsLog = glm(sold ~ ., data=train, family=binomial,control = list(maxit = 50))
summary(DescriptionWordsLog)

# And make predictions on our validation set:
PredTest = predict(DescriptionWordsLog, newdata=validate[-c(9)], type="response") #Ignoring Warning Message!

# Now we can prepare our submission file for Kaggle:
#DescriptionWordsLog = glm(sold ~ ., data=Data_Train, family=binomial)
#PredTest = predict(DescriptionWordsLog, newdata=Data_Test, type="response") #Ignoring Warning Message!
#MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)
#write.csv(MySubmission, "SubmissionDescriptionLog2.csv", row.names=FALSE)
###################################################################################################

table(train$condition)
table(validate$condition)
table(train$cellular)
table(validate$cellular)
table(train$carrier)
table(validate$carrier)
table(train$color)

#Cambiar aqui para ver si si da un AUC esprado 
library(ROCR)
predictROC=predict(DescriptionWordsLog, newdata=validate[-c(9)], type="response") #Ignoring warning here
predictROC[is.na(predictROC)] <- 0   #0.65
#predictROC[is.na(predictLog)] <- 1     #0.75
predictROC<-as.numeric(predictROC >= 0.5)
pred=prediction(predictROC,validate$sold)
perf=performance(pred,"tpr","fpr")
plot(perf)
#AUC of the CART model 

as.numeric(performance(pred, "auc")@y.values)



#PROBLEMA-> FUCKING AUC DE 1 


####################################################################################
#Approach 3 : Use a Tree!











