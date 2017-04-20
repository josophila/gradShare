#windows:
setwd('/Users/Josophila/Desktop/open_science/00_gitRepos/gradShare/')
source('./mort_funcs.R')
#modeling different outcomes for gradShare business.


#business model 1: buy properties, with a new type of lease.
#essential aspect: provide cheap rent, offset by essentially generating low-interest loans that you use to outperform.
#model it for 60 months (5 years)

####THIS DOCUMENT MODELS: 
#200k house, for which 20% was put down, a 4% 30y mortgage 
#1 tenant, who covers mortgage and propTax and pays small rent above as well as initial fee. 
#3% annual return on investment.compounded monthly
#2% yearly appreciation of home value. compounded monthly
#Every new lease (5 years) comes with a 250$ increase in initial fee
#lease is never vacant



A = matrix(nrow = 8, ncol= 360)
colnames(A) = paste("month", seq(1, ncol(A)))
catagories = c('total_assets','rent','tenentDepo', 'homeValue','remainingBal', 'profit_td-negHouseVal', 'profitTD_withHouseVal', 'cash')
rownames(A) = catagories

#############################################
###RENT COLLECTED AFTER MORTGAGE PAID
A[2,1] = 100
for(i in 2:ncol(A)){
  A[2,i] = A[2,i-1]
  if(i %% 60 == 0){
    A[2,i] = A[2,i] + 50
  }
}
#############################################
A[3,1] = 5000
for(i in 2:ncol(A)){
  A[3,i] = A[3,i-1]*(1 + 0.03/12) 
  if(i %% 60 == 0){
    A[3,i] = A[3,i] + 250
  }
}

############################################


for(i in 1:ncol(A)){
  A[4,i] = 200000*(1+.02/12)^i     #Home value appreciates 2% per year
  A[5,i] = bal(160000, .04, 30, i) #balance on debt over time
}
############################################
A[6,]

for(i in 1:ncol(A)){
  rentprof = sum(A[2,][1:i])
  invDepProf = A[3,i] 
  homeApp = A[4,i] - 200000 #current value minus original
  fees = 10000 #loss due to fees
  A[6,i] = rentprof +invDepProf - fees
  A[7,i] = rentprof + invDepProf + homeApp - fees
}
############################################
for(i in 1:ncol(A)){
  rentprof = sum(A[2,][1:i])
  invDepProf = A[3,i] 
  homeEquity = A[4,i] - A[5,i] #current value minus remaining Debt
  A[1,i] = 150000 + rentprof + invDepProf + homeEquity
}

######################################
for(i in 1:ncol(A)){
  A[8,i] = 150000+A[6,i]
}
############################################
dfmod = data.frame('TotalValue' = A[1,], 'rent' = A[2,], 'tenantDepo' = A[3,], 'homeVal' = A[4,], 'remainingBal' = A[5,], 'profit_toDate_negHomeVal' = A[6,], 'profitTD_withHomeVal' = A[7,], 'cash' = A[8,])
View(dfmod)
