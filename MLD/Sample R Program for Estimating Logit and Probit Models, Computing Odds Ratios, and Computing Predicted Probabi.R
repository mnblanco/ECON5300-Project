#Sample R Program for Estimating Binary Response Models

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location

MLD <- read.csv("MLD Data File.csv", header=TRUE)  # import data

#Print variable names on the screen
colnames(MLD)

#Install Packages if necessary
install.packages("aod")
library(aod)
library(ggplot2)
library(Rcpp)

#Generate Descriptive Statistics
summary(MLD) #take a careful look -- there are some problems

#Impose appropriate sample selection criteria here


#Estimate Logit Model
LogitModel = glm(APPROVE ~ OBRAT + BLACK + HISPAN, data = MLD, 
                 family = "binomial")
summary(LogitModel)

#Generate Odds Ratios
exp(coef(LogitModel))

#Define prototypical loan applicants (you will need more than 3)
prototype1 <- data.frame(OBRAT=mean(MLD$OBRAT),BLACK = 1, HISPAN = 0)
prototype2 <- data.frame(OBRAT=mean(MLD$OBRAT),BLACK = 0, HISPAN = 1)
prototype3 <- data.frame(OBRAT=mean(MLD$OBRAT),BLACK = 0, HISPAN = 0)

#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (LogitModel, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (LogitModel, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (LogitModel, newdata = prototype3, type ="response")

prototype1
prototype2
prototype3

#Estimate Probit Model
ProbitModel = glm(APPROVE ~ OBRAT + BLACK + HISPAN, data = MLD, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel)

#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (ProbitModel, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (ProbitModel, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (ProbitModel, newdata = prototype3, type ="response")

prototype1
prototype2
prototype3

#=========#
#== END ==#
#=========#