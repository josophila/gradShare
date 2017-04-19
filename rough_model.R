#modeling different outcomes for gradShare business.

#monthly payments on mortgage would be:
monthlyPay = function(loan, intRate, years, tax){
  moint = intRate/12
  numPay = years*12
  mopay = (loan*moint)/(1-(1+moint)^(-numPay))
  return(mopay)
}

#balance remaining function:
bal = function(loan, intRate, years, payments){
  moint = intRate/12
  numPayOwe = years*12
  balRemain = (loan*(1-(1+moint)^(payments - numPayOwe)))/(1 - (1 + moint)^(-numPayOwe))
  return(balRemain)
}

#business model 1: buy properties, with a new type of lease.
#essential aspect: provide cheap rent, offset by essentially generating low-interest loans that you use to outperform.
#model it for 60 months (5 years)

A = matrix(nrow = 5, ncol= 360)
colnames(A) = paste("month", seq(1, ncol(A)))

rownames(A) = c('total money','rent','tenentChange', 'homeValue','remainingBal')
A[1,1] = 160000
A[2,1] = 200
for(i in 1:ncol(A)){
  if(i %% 12 == 0){
    A[2,i] = 200 + 50*(i/12)
  }
  else{
    A[2,i] = A[2,(i-1)]
  }
  A[4,i] = 200000*(1+.02/12)^i
  A[5,i] = bal(160000, .04, 30, i) #balance on debt over time
}
A[2,]

if(1 %% 12 == 0){
  print('TRUE')
} else{
  print('FALSE')
}
