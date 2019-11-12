#Name: Kennard Fung
#Project: StockQuartile
#Description: The program will choose a random basket of stocks from
#a big list of stock tickers and compare the start and end results. It
#will keep doing this for a user-defined period of repetitions. Once
#completed, it will produce a density chart and a list of quartiles.

options(digits=4, width=70)

library(PerformanceAnalytics)
library(zoo)
library(tseries)
library(ggplot2)
library(Metrics)
library(tensorflow)
library(matlib)

# ***********************************************************************
# ONLY RUN THIS IF YOU DON'T HAVE THE STOCK TICKER LIST YET!

setwd("C:\Users\Ken Fung\Documents")

#grab the big csv file
big_csv = read.csv("screener.csv")
attach(big_csv)

stock_list = list()

#First, put the stocks that you want in the big list
for(x in big_csv){
  stock_list = c(stock_list, as.character(big_csv$Symbol[x]))
}

total_stock_number = 0
#Get the price list data
initial_stock_price_list = list()
later_stock_price_list = list()



for(stock in stock_list){
  tryCatch(expr = {old_stuff = get.hist.quote(instrument=stock, start="2014-01-01",
                                 end="2014-02-01", quote="AdjClose",
                                 provider="yahoo", origin="1970-01-01",
                                 compression="m", retclass="zoo")
  new_stuff = get.hist.quote(instrument=stock, start="2019-10-01",
                             end="2019-11-01", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
  initial_stock_price_list = as.numeric(c(initial_stock_price_list, as.numeric(old_stuff)))
  later_stock_price_list = as.numeric(c(later_stock_price_list, as.numeric(new_stuff)))
  total_stock_number = total_stock_number + 1
  }, silent = FALSE, condition = function(err) { }
  )
}
    

#Convert those price lists to matrices
initial_stock_price_list = as.matrix(initial_stock_price_list)
later_stock_price_list = as.matrix(later_stock_price_list)

# ***********************************************************************


start_time = Sys.time()
#USER-DEFINE: number of stocks in each basket
stocks_to_test = 10

#USER-DEFINE: Number of repetitions
repetitions = 100000

#Dev note: with an 8700k, 10 stocks and 100k repetitions takes around
#five minutes to complete. You could choose to offload the computation,
#and load it back into R right before line 110.

big_sample_change_matrix = matrix(ncol=stocks_to_test)

for(x in 1:repetitions) {
#Create the sample initial and later price lists
sample_change = list()

#Randomly pick (stocks_to_test) number of numbers
stock_sample = sample(1:total_stock_number,stocks_to_test)

#Divide up the initial capital by the number of stocks in each basket
initial_capital_matrix = list()
initial_capital = 10000

for(x in 1:stocks_to_test){
  initial_capital_matrix = as.numeric(c(initial_capital_matrix,(initial_capital/stocks_to_test)))
  
}

#Create new sample initial and later price lists
for(x in stock_sample){
  sample_change = as.numeric(c(sample_change,(later_stock_price_list[x,] / initial_stock_price_list[x,])))
  
}

#convert lists into matrices
initial_capital_matrix = as.matrix(initial_capital_matrix)
sample_change = as.matrix(sample_change)

big_sample_change_matrix = rbind(big_sample_change_matrix,t(sample_change))

}

big_sample_change_matrix = big_sample_change_matrix[-1,]

ending_matrix = tf$matmul(big_sample_change_matrix, initial_capital_matrix)

#Some sample stats!
k_mean(ending_matrix)
k_std(ending_matrix)

#Letting us know how long it took to do all those computations
end_time = Sys.time()
time_difference = end_time - start_time
time_difference

#Density function and quartile stuff
plot(density(as.numeric(ending_matrix)))

quants = list()
quants = cbind(c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99,0.999))

quant_value = list()
for(x in quants){
quant_value = c(quant_value, quantile(as.numeric(ending_matrix), x))
}

quants = cbind(quants,quant_value)
quants

