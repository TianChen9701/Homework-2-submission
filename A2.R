##Author: Tian Chen
##Date: 02/03/2022
##Purpose: Econ 613 Assignment 2

#######################
##   Question 1      ##
#######################
require(tidyverse)
#X: the age of indivduals plus intercept
#Y: Wage
#Import data
datind2009 <- read.csv("~/Desktop/Duke study/Econ613/A2/datind2009.csv")

##1. Caculate the correlation between Y and X
cor(datind2009$age, datind2009$wage, use = "complete.obs")


##2. Calculate the coefficients on this regression. remember
#Beta = (X'X)^(-1)X'Y
datind2009_nona <- datind2009 %>% select(age, wage) %>% drop_na()
age <- as.matrix(datind2009_nona$age)
one <- rep(1, length(datind2009_nona$age))
dim(one) <- c(length(datind2009_nona$age), 1)
X <- cbind(one, age)
Y <- as.matrix(datind2009_nona$wage)
beta <- solve(t(X)%*%X)%*%t(X)%*%Y
beta 


##3.1 Calculate the standard errors of beta
Y_hat <- X%*%beta
residual <- Y - Y_hat
sigma_square <- (t(residual)%*%residual)/length(residual)-2
SE_beta <- sigma_square[1,1]*solve(t(X)%*%X)
SE_constant <- sqrt(SE_beta[1,1]) 
SE_beta <- sqrt(SE_beta[2,2]) 
SE_constant
SE_beta

##3.2 Using bootstrap with 49 and 499 replications respectively. Comment on the difference between
#the two strategies.

#49 replications
num <- 1:49
data <- cbind(Y, X)
colnames(data) <- c("wage", "one", "age")
result <- matrix(1, nrow = 49, ncol = 2)
for (i in num) {
  sample <- data[sample(nrow(data), 10000), ]
  beta <- solve(t(sample[,2:3])%*%sample[,2:3])%*%t(sample[,2:3])%*%sample[,1]
  result[i,1] <- beta[1,1]
  result[i,2] <- beta[2,1]
}
#bootstrap constant: 49 replication
mean(result[,1])
#bootstrap beta
mean(result[,2])

#499 replications
num <- 1:499
result <- matrix(1, nrow = 499, ncol = 2)
for (i in num) {
  sample <- data[sample(nrow(data), 10000), ]
  beta <- solve(t(sample[,2:3])%*%sample[,2:3])%*%t(sample[,2:3])%*%sample[,1]
  result[i,1] <- beta[1,1]
  result[i,2] <- beta[2,1]
}
#bootstrap constant: 499 replication
mean(result[,1])
#bootstrap beta: 499 replication
mean(result[,2])

######################
##    Question 2    ##
######################
#Import data
rm(list = ls())
datind_file <- list.files(pattern = "^datind")
datind_combined <- read.csv(datind_file[1])
datind_file <- datind_file[-1]
for(file in datind_file){
  csv <- read.csv(file)
  datind_combined <- rbind(datind_combined, csv)
}
datind_combined_2005_2018 <- datind_combined %>% as_tibble() %>% filter(year>2004&year<2019)  

##1.Create a categorical variable ag, which bins the age variables into the following groups: “18-25”, “26-
#30”, “31-35”, “36-40”,“41-45”, “46-50”,“51-55”, “56-60”, and “60+”.
datind_combined_2005_2018 <- datind_combined_2005_2018 %>% mutate(ag = as.factor(ifelse(18 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 25 , '18-25',
                                                                                             ifelse(26 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 30, '26-30',
                                                                                                    ifelse(31 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 35, '31-35', 
                                                                                                           ifelse(36 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 40, '36-40',
                                                                                                                  ifelse(41 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 45, "41-45",
                                                                                                                         ifelse(46 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 50, "46-50",
                                                                                                                                ifelse(51 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 55, "51-55",
                                                                                                                                       ifelse(56 <= datind_combined_2005_2018$age & datind_combined_2005_2018$age<= 60, "56-60", "60+"))))))))))


##2.Plot the wage of each age group across years. Is there a trend?
plot(datind_combined_2005_2018$ag, datind_combined_2005_2018$wage)
#trend: with age increasing, wage concentrates on higher income

##3.Fixed effect model
#we use fast dummy package to create time fixed effect dummy
#install.packages("fastDummies")
require(fastDummies)
datind_combined_2005_2018 <- dummy_cols(datind_combined_2005_2018,select_columns = "year")
data <- datind_combined_2005_2018 %>% select(wage, age, year_2005:year_2018) %>% drop_na()
X <- as.matrix(select(data, age, year_2005:year_2018))
Y <- as.matrix(select(data, wage))
results <- solve(t(X)%*%X)%*%t(X)%*%Y
results
#the OLS results underestimate the coefficient of age


########################
##      Question3     ##
########################
datind_2007 <- read.csv("~/Desktop/Duke study/Econ613/A2/datind2007.csv")
datind_2007_tbl <- datind_2007 %>% as_tibble()

##1. exclude inactive respondents
datind_2007_wage_age <- datind_2007_tbl %>% select(wage, age) %>% drop_na()


##2. 
# Decide feature and label
df_tbl <- df %>% as_tibble() %>% select(empstat, age, wage) %>% drop_na() 
df_tbl <- df_tbl %>% mutate(employ_dummy = ifelse(df_tbl$empstat == "employed", 1,0 ))
X <- as.matrix(df_tbl$age)
Y <- as.matrix(df_tbl$employ_dummy)

# Likelihood Function
Likelihood <- function(X = X, y = Y, beta){
  num <- 1:length(X)
  L = 0
  for (i in num) {
    x_i = X[i,]
    y_i = y[i]
    L = L * ((pnorm(x_i%*%beta,0,1))^y_i) * ((1- pnorm(x_i%*%beta,0,1))^(1-y_i))
  }
  return(L)
}


##Maximization
list_1 = seq(-100,100, by = 1)
list_2 = seq(-100,100, by = 1)

best_LH = 0
best_beta = c(0,0)
for (i in list_1){
  for (j in list_2){
    beta = c(i,j)
    LHi = LH(X = X, y = y, beta = beta)
    if (LHi>=best_LH){
      best_LH = LHi
      best_beta = beta
    }
    else{}
  }
}


###################
##  Question4  ``##
###################
##Exclude all individuals who are inactive.
datind_2005_2015 <- datind_combined %>% filter(year >= 2005 & year <= 2015)
datind_2005_2015 <- dummy_cols(datind_2005_2015,select_columns = "year") %>% select(age, empstat, year) %>% drop_na()
#empstat: individual’s participation in the labor market
df <- datind_2005_2015 %>%mutate(employ_dummy = ifelse(datind_2005_2015$empstat == "Employed", 1,0), year_factor = as.factor(year))

##Write and optimize the probit, logit, and the linear probability models.
##OLS, probit, logit
require(glm2)

#LPM
LPM <- lm(employ_dummy~age + year_factor, data = df)
summary(LPM)
#probit
Probit <- glm(employ_dummy~age + year_factor, data = df, family = binomial(link = "probit"))
summary(Probit)
#logit
Logit <- glm(employ_dummy~age + year_factor, data = df, family = binomial(link = "logit"))
summary(Logit)

##Interpret and compare the estimated coefficients. How significant are they?
#All model's coefficient are statistically significant at 5 % level, but the linear probability model
#returns the extremely small coefficient. In conclusion, age is positively correlated with employment
#Older people are morelily to get a job.


#######################
##    Question 5     ##
#######################
#install.packages("ggeffects")
#install.packages("prediction")
require(ggeffects, prediction)


##Marginal effects
#LPM
LPM_effects <- ggpredict(LPM, "age")
LPM_effects
#Probit
Probit_effects <- ggpredict(Probit, "age")
Probit_effects
#Logit
Logit_effects <- ggpredict(Logit, "age")
Logit_effects

##Standard error of marginal effects
#LPM
SD_LPM_effects <- cbind(as.matrix(LPM_effects)[,1], as.matrix(LPM_effects)[,3])
SD_LPM_effects
#Probit
SD_Probit_effects <- cbind(as.matrix(Probit_effects)[,1], as.matrix(Probit_effects)[,3])
SD_Probit_effects
#Logit
SD_Logit_effects <- cbind(as.matrix(Logit_effects)[,1], as.matrix(Logit_effects)[,3])
SD_Logit_effects











