#Mortgage Functions

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