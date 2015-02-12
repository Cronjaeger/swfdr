set.seed(1)
a = 50
b = (1/0.8 - 1)*a


x=seq(0,1,length.out=100)
plot(x,dbeta(x,a,b),type='l',col=1,xlab=NA,ylab='Density',bty='n')
polygon(x,dbeta(x,a,b),col=rgb(1, 0, 0,0.5))

meanB = a / (a+b)
varB = a*b / ( (a+b)^2 * (a+b+1))
stdB = sqrt(varB)


lowerB = qbeta(0.025,a,b)
upperB = qbeta(0.975,a,b)

print(lowerB)
print(upperB)



a = 17
b = (1/0.25 - 1)*a


x=seq(0,1,length.out=100)
lines(x,dbeta(x,a,b),type='l',col=1)


meanB = a / (a+b)
varB = a*b / ( (a+b)^2 * (a+b+1))
stdB = sqrt(varB)
polygon(x,dbeta(x,a,b),col=rgb(0, 1, 0,0.5))



lowerB = qbeta(0.025,a,b)
upperB = qbeta(0.975,a,b)

print(lowerB)
print(upperB)







a = 40
b = (1/0.4 - 1)*a


x=seq(0,1,length.out=100)
lines(x,dbeta(x,a,b),type='l',col=1)
polygon(x,dbeta(x,a,b),col=rgb(0,0,1,0.5))

meanB = a / (a+b)
varB = a*b / ( (a+b)^2 * (a+b+1))
stdB = sqrt(varB)


lowerB = qbeta(0.025,a,b)
upperB = qbeta(0.975,a,b)

print(lowerB)
print(upperB)


legend(locator(1), inset=0.33, title="Prior densities",c(expression(p(symbol(beta))),expression(p(bar(H[0]))),expression(p(u))), fill=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)), cex=0.8)











powerA = 60
powerB = 15

pTA = 15
pTB = 45

uA = 40
uB = 60

alpha = 0.05


n = 1e5
power = rbeta(n,powerA,powerB)
pT = rbeta(n,pTA,pTB)
u = rbeta(n,uA,uB)


k = power + u*(1-power)


PPV = k*pT / (k*pT + (1-pT)*((1-u)*alpha + u))

hist(PPV,40,col=8,main=NULL)
rug(PPV)
abline(v=0.5,col=4,lty=2)


print(sum(PPV>0.5)/n)


