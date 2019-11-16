#Name: Kennard Fung
#Project: MinVarPort
#Description: As of right now, this portfolio will give the optimal portfolio weights
#that would yield the highest returns for the lowest risk (minimum variance portfolio).
#I still need to clean and edit some stuff
#TO-DO
#1. Generalize for any NYSE stock
#2. Show mu and sigma
#3. Add comments for clarity


options(digits=4, width=70)

library(PerformanceAnalytics)
library(zoo)

#First grab stock market data

stock_count = 5

AAPL.prices = get.hist.quote(instrument="aapl", start="2009-01-01",
                             end="2018-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

index(AAPL.prices) = as.yearmon(index(AAPL.prices))

AMD.prices = get.hist.quote(instrument="amd", start="2009-01-01",
                             end="2018-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(AMD.prices) = as.yearmon(index(AMD.prices))

ORCL.prices = get.hist.quote(instrument="orcl", start="2009-01-01",
                            end="2018-12-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
index(ORCL.prices) = as.yearmon(index(ORCL.prices))

GOOGL.prices = get.hist.quote(instrument="googl", start="2009-01-01",
                            end="2018-12-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
index(GOOGL.prices) = as.yearmon(index(GOOGL.prices))

NVDA.prices = get.hist.quote(instrument="nvda", start="2009-01-01",
                             end="2018-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

index(NVDA.prices) = as.yearmon(index(NVDA.prices))


market_prices = c()

market_prices = merge(AAPL.prices,AMD.prices,GOOGL.prices,ORCL.prices,NVDA.prices)

colnames(market_prices) = c("AAPL","AMD","GOOGL","ORCL","NVDA")

market_prices = diff(log(market_prices))

mktprice_mat = coredata(market_prices)

colnames(mktprice_mat)

mktprice_covmat = cov(mktprice_mat)
class(mktprice_covmat)

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

mktprice_covmat = cbind(mktprice_covmat,matcol)
mktprice_covmat = rbind(mktprice_covmat,matrow)

sumlist_mat = matrix(sumlist,ncol=1)

unknown_mat = inv(mktprice_covmat) %*% sumlist_mat
unknown_mat = cbind(unknown_mat, c("AAPL","AMD","GOOGL","ORCL","NVDA","LagMult"))
unknown_mat






