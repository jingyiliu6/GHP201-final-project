#GHP201 - Session: Modeling financial risk protection in health - Part II
#April 2, 2026
#Instructor: Stéphane Verguet

set.seed(123)
######
#1 Simulating a distribution of income - giving one example
######

n <- 1000	         #assign a number of households in the population
y <- NULL 		 #assign a vector of incomes

mu <- 1000
sd <- 500

y <- rgamma(n,(mu^(2))/(sd^(2)),1/(sd^(2)/mu))	#assign a gamma distribution of income

qt <- quantile(y, probs=seq(0.20,1, by=0.20), na.rm=TRUE) 
#extract the income cutoffs for each quintile

incomes_1 <- y[y<qt[1]]
incomes_2 <- y[y>=qt[1] & y<qt[2]]
incomes_3 <- y[y>=qt[2] & y<qt[3]]
incomes_4 <- y[y>=qt[3] & y<qt[4]]
incomes_5 <- y[y>=qt[4]]
#extract income values for each quintile

plot(sort(y),main="Ranking incomes",xlab='Rank',ylab='Income ($)',pch=19,col='purple')	
#plot ranks incomes
abline(h=qt,lty=3,col='grey',lwd=2)
legend(1,max(y),
legend=c("Income quintile cutoffs"),lty=3,lwd=2,col=c("grey"),bty="n",cex=1.0)


######
#2 Computing illness-related expenditures
######

#2.1 Annual OOP direct medical expenditures tied to treatment of disease D

#Assigning OOP estimates for each household among the n households
#for example, assume oop expenditures that increase linearly 
#with income level between two extremes of 100 and 200

oopmin <- 100
oopmax <- 200

oop <- seq(oopmin,oopmax,length.out=n) 

#2.2 Consider health services utilization and compute those seeking care
#Only a certain number of individuals seek care for treatment

ut <- c(0.20,0.30,0.40,0.50,0.70)	
#health services utilization gradient: 
#only a certain fraction of individuals seeks care 
#(this is generally given from household survey data, e.g. DHS)

seekers_1 <- sample(incomes_1,length(incomes_1)*ut[1])
seekers_2 <- sample(incomes_2,length(incomes_2)*ut[2])
seekers_3 <- sample(incomes_3,length(incomes_3)*ut[3])
seekers_4 <- sample(incomes_4,length(incomes_4)*ut[4])
seekers_5 <- sample(incomes_5,length(incomes_5)*ut[5])
#extract those individuals who will seek care and their incomes 

oop_1 <- sample(oop[1:200],length(incomes_1)*ut[1])
oop_2 <- sample(oop[201:400],length(incomes_2)*ut[2])
oop_3 <- sample(oop[401:600],length(incomes_3)*ut[3])
oop_4 <- sample(oop[601:800],length(incomes_4)*ut[4])
oop_5 <- sample(oop[801:1000],length(incomes_5)*ut[5])
#extract those individuals who will seek care and their OOP costs 

######
#3. Simulating illness-related impoverishment
######

#3.1 Estimating catastrophic health expenditures

threshold <- 0.10		#set catastrophic threshold of income

#OOP costs only for CHE incidence

cat_1 <- sum(oop_1>threshold*seekers_1)
cat_2 <- sum(oop_2>threshold*seekers_2)
cat_3 <- sum(oop_3>threshold*seekers_3)
cat_4 <- sum(oop_4>threshold*seekers_4)
cat_5 <- sum(oop_5>threshold*seekers_5)
#compute number of CHE cases per quintile

i_cat_1 <- 100*cat_1/length(incomes_1)
i_cat_2 <- 100*cat_2/length(incomes_2)
i_cat_3 <- 100*cat_3/length(incomes_3)
i_cat_4 <- 100*cat_4/length(incomes_4)
i_cat_5 <- 100*cat_5/length(incomes_5)

cat_oop <- c(i_cat_1,i_cat_2,i_cat_3,i_cat_4,i_cat_5)

barplot(cat_oop,space=2,main="% of households with CHE",
        xlab='Income quintile (from poorest to richest)',
        ylab='% of households per quintile',col='purple')

#3.2 Estimating impoverishing health expenditures

pov <- 1.90*365			#set poverty threshold (e.g. $1.90 per day)

#OOP costs only for IHE incidence

pov_1 <- sum(seekers_1>pov&(seekers_1-oop_1<pov))
pov_2 <- sum(seekers_2>pov&(seekers_2-oop_2<pov))
pov_3 <- sum(seekers_3>pov&(seekers_3-oop_3<pov))
pov_4 <- sum(seekers_4>pov&(seekers_4-oop_4<pov))
pov_5 <- sum(seekers_5>pov&(seekers_5-oop_5<pov))
#compute IHE cases per quintile

i_pov_1 <- 100*pov_1/length(incomes_1)
i_pov_2 <- 100*pov_2/length(incomes_2)
i_pov_3 <- 100*pov_3/length(incomes_3)
i_pov_4 <- 100*pov_4/length(incomes_4)
i_pov_5 <- 100*pov_5/length(incomes_5)

pov_oop <- c(i_pov_1,i_pov_2,i_pov_3,i_pov_4,i_pov_5)

barplot(pov_oop,space=2,main="% of households with IHE",
        xlab='Income quintile (from poorest to richest)',
        ylab='% of households per quintile',col='purple')

######
#4. Value of insurance
######

#utility function

u <- function(x,rra)
{
1/(1-rra)*x^(1-rra)
}
#constant relative risk aversion (CRRA) utility function
#x is income
#rra is coefficient of relative risk aversion

u_inv <- function(x,rra)
{
((1-rra)*x)^(1/(1-rra))
}
#inverse of CRRA utility function

#expected value of income 
yp <- function(x,p,cost)
{
p*(x-cost)+(1-p)*x	
}
#p is probability of "event"
#cost is cost of "event"

#expected value of utility 
up <- function(x,p,cost,rra)
{
p*u(x-cost,rra)+(1-p)*u(x,rra)
}

#insurance value
ins <- function(x,p,cost,rra)
{
yp(x,p,cost)-u_inv(up(x,p,cost,rra),rra)
}

#general inputs
p1 <- 0.10 		#disease incidence
cost1 <- 100		#OOP direct medical costs
rra1 <- 3               #risk aversion

ins1 <- ins(sort(y),p1,cost1,rra1)
ins2 <- ins(sort(y),p1,cost1,rra1/2)

plot(ins1,main="Value of insurance",xlab='Income (from poorest to richest)',
     ylab='risk premium',col='purple',type='l',xlim=c(0,600),lwd=3)
points(ins2,col='blue',type='l',lwd=3)

######
#5. Borrowing and debt
######

#borrow amount of money l (=cost for health care) at interest rate r over n=years 
#annual payment of loan
a <- function(l,r,years)
{
l*r/(1-(1+r)^(-years))
}
#l is cost
#r is interest rate
#years is number of years for loan repayment

#debt (net present value; each annual cash flow is discounted back to its present value)
debt <- function(a,q,years)
{
sum(a/(1+q)^(1:years))	
}
#q is discount rate

#example with general inputs
l1 <- 200 #cost of $200
years1 <- 10 #repayment over 10 years
q1 <- 0.03 #discount rate of 3% per year
r1 <- 0.20 #interest rate of 20% per year

a(l1,r1,years1) #annual payment
debt(a(l1,r1,years1),q1,years1) #total debt 


