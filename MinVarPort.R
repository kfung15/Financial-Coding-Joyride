#Name: Kennard Fung
#Project: MinVarPort
#Description: As of right now, this portfolio will give the optimal portfolio weights
#that would yield the highest returns for the lowest risk (minimum variance portfolio).
#I still need to clean and edit some stuff
#TO-DO
#1. Generalize for any NYSE stock
#2. Add (more) comments for clarity


options(digits=4, width=70)

library(PerformanceAnalytics)
library(zoo)
library(matlib)
library(tseries)
library(tensorflow)
library(tidyverse)
library(keras)

#First grab stock market data

stock_count = 3

PG.prices = get.hist.quote(instrument="pg", start="2006-01-01",
                            end="2018-12-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
index(PG.prices) = as.yearmon(index(PG.prices))

SBUX.prices = get.hist.quote(instrument="sbux", start="2006-01-01",
                            end="2018-12-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
index(SBUX.prices) = as.yearmon(index(SBUX.prices))

LMT.prices = get.hist.quote(instrument="lmt", start="2006-01-01",
                             end="2018-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

index(LMT.prices) = as.yearmon(index(LMT.prices))


market_prices = c()

market_prices = merge(PG.prices,SBUX.prices,LMT.prices)

colnames(market_prices) = c("PG","SBUX","LMT")

market_prices = diff(log(market_prices))

mktprice_mat = coredata(market_prices)

colnames(mktprice_mat)

mktprice_covmat = cov(mktprice_mat)
class(mktprice_covmat)

#find the means of each stock
mu.p = c()
for(x in 1:stock_count){
  mu.p = c(mu.p,median(mktprice_mat[,x]))
}
mu.p = as.matrix(mu.p)

matrow = c()
matcol = c()
sumlist = c()

for(x in 1:stock_count){
   matcol = c(matcol, -1)
   matrow = c(matrow, 1)
   sumlist = c(sumlist,0)
}

matrow = c(matrow, 0)
sumlist = c(sumlist,1)

#Adjust the covariance matrix for future calculations
mktprice_covmat_new = c()
mktprice_covmat_new = cbind(mktprice_covmat,matcol)
mktprice_covmat_new = rbind(mktprice_covmat_new,matrow)

sumlist_mat = matrix(sumlist,ncol=1)

unknown_mat = inv(mktprice_covmat_new) %*% sumlist_mat
unknown_mat_final = cbind(unknown_mat, c("PG","SBUX","LMT","LagMult"))
unknown_mat_final

#Unknown mat without the lagrangian multiplier
unknown_mat_pure = unknown_mat[1:stock_count]
unknown_mat_pure = as.matrix(unknown_mat_pure)

#Output the risk and reward figures for the minimum variance portfolio
initial_reward = t(mu.p) %*% unknown_mat_pure
initial_reward

initial_risk = (t(unknown_mat_pure) %*% mktprice_covmat) %*% (unknown_mat_pure)
initial_risk

#Next step, finding the percentage that you want!

#First, create a bunch of random portfolio weight

step_count = 10000

#Create random numbers from a normal distribution for each of the stocks
x1 = runif(step_count, min=-0.5, max = 1.5)
x2 = runif(step_count, min=-0.5, max = 1.5)
x3 = 1 - x1 - x2

x4 = c(x1,x2,x3)
x4 = matrix(unlist(x4),nrow=step_count,ncol=stock_count)

#Grab the relevant risk figures from the diagonal of the result
risk_list = c()
risk_list = tf$matmul(x4, mktprice_covmat) 
risk_list = tf$matmul(risk_list, t(x4))
risk_list = tf$linalg$diag_part(risk_list)

#Reshape the risk list so that col = 1
risk_list = k_reshape(risk_list, c(step_count,1))

#-------------------------------------------------------------

#Reward

reward_list = tf$matmul(x4,mu.p)
reward_list

#-------------------------------------------------------------

#Combine the two lists
overall_list = k_concatenate(c(reward_list,risk_list),axis=2)

#Change into a matrix
overall_list = k_eval(overall_list)

#Change into a dataframe
overall_list = as.data.frame(overall_list)

#Give the overall_list some column names for easy reference
colnames(overall_list) = c("reward", "risk")

#Sort the overall_list by reward, ascending order
overall_sorted = overall_list[order(overall_list$reward),]

#Give me all the risk points for a monthly reward of 1% and above
overall_sorted = overall_sorted %>% filter(reward >= 0.01)

#Round the reward column to 4 decimal places
overall_sorted[,1] = round(overall_sorted[,1],4)

#Convert overall_sorted to a dataframe
overall_sorted = as.data.frame(overall_sorted)

#Convert the reward column into a factor
overall_sorted$reward = factor(overall_sorted$reward)

#Return the minimum risk for each reward factor
overall_perfect = overall_sorted %>% group_by(reward) %>% summarize(risk = min(risk))

#Plot the risk-return points
plot(overall_perfect)


