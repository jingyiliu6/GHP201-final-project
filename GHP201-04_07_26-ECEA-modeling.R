#GHP201 - Session: Modeling approaches for ECEA - public finance of TB treatment and rotavirus vaccine
#Instructor: Stéphane Verguet
#April 7, 2026

data <- read.csv('tb-inputs.csv',header=TRUE) 

set.seed(123)

#Example A: raise coverage of TB treatment to 100% in the whole population

#1. Health benefits per income quintile (simple static model, no dynamic effects)

#1.1. Estimating number of TB-related deaths occuring before intervention in each income quintile

deaths_baseline <- c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inc_1"]*(data$value[data$parameter=="cov_1"]*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov_1"])*data$value[data$parameter=="cfr"])
                     ,data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inc_2"]*(data$value[data$parameter=="cov_2"]*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov_2"])*data$value[data$parameter=="cfr"]),
                     data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inc_3"]*(data$value[data$parameter=="cov_3"]*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov_3"])*data$value[data$parameter=="cfr"]),
                     data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inc_4"]*(data$value[data$parameter=="cov_4"]*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov_4"])*data$value[data$parameter=="cfr"]),
                     data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inc_5"]*(data$value[data$parameter=="cov_5"]*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov_5"])*data$value[data$parameter=="cfr"]))
#note that both case fatality ratio (CFR) and treatment effectiveness 
#are assumed to be constant per quintile

#1.2. Estimating number of TB-related deaths occuring after intervention in each quintile

deaths_intervention <- c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inc_1"]*(1*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-1)*data$value[data$parameter=="cfr"]),
                         data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inc_2"]*(1*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-1)*data$value[data$parameter=="cfr"]),
                         data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inc_3"]*(1*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-1)*data$value[data$parameter=="cfr"]),
                         data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inc_4"]*(1*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-1)*data$value[data$parameter=="cfr"]),
                         data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inc_5"]*(1*data$value[data$parameter=="cfr"]*(1-data$value[data$parameter=="eff"])+(1-1)*data$value[data$parameter=="cfr"]))	
	
#1.3. Estimating the number of TB-related deaths averted per quintile, 
#and plotting TB-related deaths averted per quintile

deaths_averted <- deaths_baseline - deaths_intervention			

barplot(deaths_averted,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Deaths averted',main='TB-related deaths averted',col='red',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))

#2. OOP costs crowded out per income quintile

#Estimating number of TB-related OOP costs occuring before intervention 
#in each income quintile and hence being crowded out by public finance

oop_baseline <- c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inc_1"]*data$value[data$parameter=="cov_1"]*data$value[data$parameter=="oop_1"],
                  data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inc_2"]*data$value[data$parameter=="cov_2"]*data$value[data$parameter=="oop_2"],
                  data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inc_3"]*data$value[data$parameter=="cov_3"]*data$value[data$parameter=="oop_3"],
                  data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inc_4"]*data$value[data$parameter=="cov_4"]*data$value[data$parameter=="oop_4"],
                  data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inc_5"]*data$value[data$parameter=="cov_5"]*data$value[data$parameter=="oop_5"])	
	
barplot(oop_baseline,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Out-of-pocket costs averted',main='TB-related out-of-pocket expenditures averted',
        col='orange',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))

#3. FRP benefits per income quintile

#3.1. Simulating incomes from income distribution and assigning to individuals seeking TB care

#generating an income distribution from average income and gini
gini <- data$value[data$parameter=="gini"]
ave_income <- data$value[data$parameter=="mean"]

phi <- 1/gini
beta <- (1/phi)*(ave_income)

incomes <- rgamma(n=1000000,phi,rate=1/beta)
#n=total population = 1,000,000

qt <- quantile(sort(incomes), probs=seq(0,1, by=0.20), na.rm=TRUE) 
#extracting income quintile cutoffs

pop_incomes <- matrix(NA,5,data$value[data$parameter=="pop"]/5)

for(i in 1:5)
{
pop_incomes[i,] <- sample(incomes[incomes<=qt[i+1]&incomes>=qt[i]],data$value[data$parameter=="pop"]/5)	
}
#generating incomes for each individual per quintile

pop_incomes_u_1 <- sample(pop_incomes[1,],
data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inc_1"]*data$value[data$parameter=="cov_1"])	
pop_incomes_u_2 <- sample(pop_incomes[2,],
data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inc_2"]*data$value[data$parameter=="cov_2"])	
pop_incomes_u_3 <- sample(pop_incomes[3,],
data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inc_3"]*data$value[data$parameter=="cov_3"])	
pop_incomes_u_4 <- sample(pop_incomes[4,],
data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inc_4"]*data$value[data$parameter=="cov_4"])	
pop_incomes_u_5 <- sample(pop_incomes[5,],
data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inc_5"]*data$value[data$parameter=="cov_5"])	
#generating incomes for each individual per quintile, who has TB and utilizes care

#3.2. Estimating catastrophic health expenditures averted

th <- data$value[data$parameter=="th"]
#setting catastrophic threshold of income 

cata_baseline <- c(sum(data$value[data$parameter=="oop_1"]>th*pop_incomes_u_1),
                   sum(data$value[data$parameter=="oop_2"]>th*pop_incomes_u_2),
                   sum(data$value[data$parameter=="oop_3"]>th*pop_incomes_u_3),
                   sum(data$value[data$parameter=="oop_4"]>th*pop_incomes_u_4),
                   sum(data$value[data$parameter=="oop_5"]>th*pop_incomes_u_5))  
#counting the number of times OOP costs surpass the threshold of income

barplot(cata_baseline,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Cases of CHE averted',main='TB CHE cases averted',col='purple',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))
#plotting TB_related CHE cases averted

#4. Total costs of the policy to public sector

policy_costs <- data$value[data$parameter=="c"]*c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inc_1"]*1,
                                                  data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inc_2"]*1,
                                                  data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inc_3"]*1,
                                                  data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inc_4"]*1,
                                                  data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inc_5"]*1)
#estimating total policy costs per quintile, before treatment switches from private 
#financing to public financing (pre-coverage), and new treatment fully publically financed 
#(post-coverage - pre-coverage)
#note: unit cost of TB treatment assumed constant per quintile

#5. Summarizing ECEA outcomes

#5.1. Dashboard
ecea_dash <- as.data.frame(rbind(round(deaths_averted),round(oop_baseline),round(cata_baseline)))
colnames(ecea_dash) <- c("I","II","III","IV","V")
rownames(ecea_dash) <- c("Deaths averted","OOP expenditures averted","CHE cases averted")

#5.2. Efficiency plot (per $1M spent)

x <- ((deaths_averted/policy_costs)*1000000)
y <- ((cata_baseline/policy_costs)*1000000)
plot(y~x,main='Health and FRP benefits from UPF of TB treatment, per $1M spent',
     pch=19,type='b',lty=3,xlim=c(0.9*min(x),max(x)*1.1),ylim=c(0,max(y)*1.1),
     xlab='Deaths averted',ylab='Cases of catastrophic health expenditures averted')


##################################
#Example B - Public finance rotavirus immunization
##################################

data <- read.csv('rv-inputs.csv',header=TRUE)		#Load data csv file into the R console

#Policy: universal public finance
#Intervention = rotavirus immunization of newborns

#I-Health benefits per income quintile

deaths_baseline <- data$value[data$parameter=="births"]*data$value[data$parameter=="inc"]*1/5*c(data$value[data$parameter=="r1"],
                                                                                                data$value[data$parameter=="r2"],
                                                                                                data$value[data$parameter=="r3"],
                                                                                                data$value[data$parameter=="r4"],
                                                                                                data$value[data$parameter=="r5"])
#distributing the number of rotavirus-related deaths occuring before vaccines among 
#under-five children per income quintile using a risk index approach

deaths_intervention <- data$value[data$parameter=="births"]*data$value[data$parameter=="inc"]*1/5*c(data$value[data$parameter=="r1"],
                                                                                                    data$value[data$parameter=="r2"],
                                                                                                    data$value[data$parameter=="r3"],
                                                                                                    data$value[data$parameter=="r4"],
                                                                                                    data$value[data$parameter=="r5"])*(data$value[data$parameter=="cov"]*(1-data$value[data$parameter=="eff"])+(1-data$value[data$parameter=="cov"]))
 	
#estimating the number of rotavirus-related deaths occuring after vaccines in each income quintile
#note that both coverage (cov) and vaccine effectiveness (eff) are assumed to be constant 
#per income quintile

deaths_averted <- deaths_baseline - deaths_intervention			
#estimating the number of rotavirus-related deaths averted per income quintile

barplot(deaths_averted,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Deaths averted',main='Rotavirus-related deaths averted',col='red',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))
#plotting the rotavirus-related deaths averted per income quintile

#II-Private expenditures crowded out per income quintile
oop_intervention <- c(data$value[data$parameter=="pop_1"]*(data$value[data$parameter=="inp1"]*data$value[data$parameter=="cov_1"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="oop_inp1"]+data$value[data$parameter=="out1"]*data$value[data$parameter=="cov_1"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="oop_out1"]),
                      data$value[data$parameter=="pop_2"]*(data$value[data$parameter=="inp2"]*data$value[data$parameter=="cov_2"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="oop_inp2"]+data$value[data$parameter=="out2"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_2"]*data$value[data$parameter=="oop_out2"]),
                      data$value[data$parameter=="pop_3"]*(data$value[data$parameter=="inp3"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_3"]*data$value[data$parameter=="oop_inp3"]+data$value[data$parameter=="out3"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_3"]*data$value[data$parameter=="oop_out3"]),
                      data$value[data$parameter=="pop_4"]*(data$value[data$parameter=="inp4"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_4"]*data$value[data$parameter=="oop_inp4"]+data$value[data$parameter=="out4"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_4"]*data$value[data$parameter=="oop_out4"]),
                      data$value[data$parameter=="pop_5"]*(data$value[data$parameter=="inp5"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_5"]*data$value[data$parameter=="oop_inp5"]+data$value[data$parameter=="out5"]*data$value[data$parameter=="eff"]*data$value[data$parameter=="cov_5"]*data$value[data$parameter=="oop_out5"]))		
#estimating the number of rotavirus-related out-of-pocket expenditures occuring before 
#intervention in each income quintile and hence being crowded out by universal public finance 
#of rotavirus vaccine with a certain effectiveness

barplot(oop_intervention,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Out-of-pocket costs averted',
        main='Rotavirus-related out-of-pocket expenditures averted',col='orange',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))
#plotting the rotavirus-related out-of-pocket expenditures averted per income quintile

#III-Financial risk protection benefits per income quintile

#generating an income distribution (more sophisticated version than above)
gini <- data$value[data$parameter=="gini"]
ave_income <- data$value[data$parameter=="mean"]

fgamma <- function(phi){gini-(1/(phi*4^phi))*1/beta(phi,phi + 1)} 
#income distribution function
phi <- uniroot(fgamma,lower=0.000001,upper=100)$root
beta <- (1/phi)*(ave_income)

income <- function(y){dgamma(y,shape=phi,scale=beta)}

plot(income,0,2000,main='Income distribution',ylab='Density',xlab='Annual income ($)')
#plotting the income distribution 
 
tile <- function(x)
{
uniroot(function(y)
{
x-integrate(income,lower=0,upper=y)$value
}
,lower=0,upper=1000000)$root 
}
#alternative function to generate income cutoffs
 
qt <- c(tile(0.2),tile(0.4),tile(0.6),tile(0.8))
#income quintile cutoffs

incomes <- rgamma(6000000,phi,rate=1/beta)
#generating 6 millions incomes from the income distribution

pop_incomes <- matrix(NA,5,data$value[data$parameter=="births"]/5)
for(i in 2:4)
{
pop_incomes[i,] <- sample(incomes[incomes<qt[i]&incomes>qt[i-1]],data$value[data$parameter=="births"]/5)	
}
pop_incomes[1,] <- sample(incomes[incomes<qt[1]],data$value[data$parameter=="births"]/5)
pop_incomes[5,] <- sample(incomes[incomes>qt[4]],data$value[data$parameter=="births"]/5)
#generating incomes for each individual in each income quintile

pop_incomes_u_1 <- sample(pop_incomes[1,],
data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inp1"])	
pop_incomes_u_2 <- sample(pop_incomes[2,],
data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inp2"])	
pop_incomes_u_3 <- sample(pop_incomes[3,],
data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inp3"])	
pop_incomes_u_4 <- sample(pop_incomes[4,],
data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inp4"])	
pop_incomes_u_5 <- sample(pop_incomes[5,],
data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inp5"])	
#generating incomes for each individual in each income quintile which has rotavirus and 
#utilizes inpatient healthcare (where OOP costs are large)

#III-1-Estimation of catastrophic health expenditures averted

th <- data$value[data$parameter=="th"]
#setting catastrophic threshold of income 

cata_baseline <- c(sum(data$value[data$parameter=="oop_inp1"]>th*pop_incomes_u_1),
                   sum(data$value[data$parameter=="oop_inp2"]>th*pop_incomes_u_2),
                   sum(data$value[data$parameter=="oop_inp3"]>th*pop_incomes_u_3),
                   sum(data$value[data$parameter=="oop_inp4"]>th*pop_incomes_u_4),
                   sum(data$value[data$parameter=="oop_inp5"]>th*pop_incomes_u_5))  
#counting the number of times the inpatient OOP expenditures surpass the set 
#threshold of income

barplot(cata_baseline,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Rotavirus-related cases of catastrophic health expenditures averted',
        main='Cases of catastrophic health expenditures averted',col='purple',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))
#plotting the rotavirus_related catastrophic health expenditures averted

inc_cata_baseline <- cata_baseline/c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inp1"],
                                     data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inp2"],
                                     data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inp3"],
                                     data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inp4"],
                                     data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inp5"])*100
#incidence of cases of catastrophic health expenditures among those with seeking inpatient care

#III-2-Estimation of cases of medical impoverishment averted

pov <- data$value[data$parameter=="pov"]
#poverty line 

pov_baseline <- c(sum((pop_incomes_u_1-data$value[data$parameter=="oop_inp1"]<pov)&(pop_incomes_u_1>pov)),
                  sum((pop_incomes_u_2-data$value[data$parameter=="oop_inp2"]<pov)&(pop_incomes_u_2>pov)),
                  sum((pop_incomes_u_3-data$value[data$parameter=="oop_inp3"]<pov)&(pop_incomes_u_3>pov)),
                  sum((pop_incomes_u_4-data$value[data$parameter=="oop_inp4"]<pov)&(pop_incomes_u_4>pov)),
                  sum((pop_incomes_u_5-data$value[data$parameter=="oop_inp5"]<pov)&(pop_incomes_u_5>pov)))  
#counting the number of times the inpatient out-of-pocket expenditures push the 
#incomes under the poverty line

barplot(pov_baseline,beside=TRUE,xlab='Income quintile (poorest to richest)',
        ylab='Rotavirus-related cases of medical impoverishment averted',
        main='Cases of poverty averted',col='purple',xaxt='n')
axis(1,c(1,2,3,4,5),c("I","II","III","IV","V"))
#plotting the rotavirus_related cases of poverty averted

inc_pov_baseline <- pov_baseline/c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="inp1"],
                                   data$value[data$parameter=="pop_2"]*data$value[data$parameter=="inp2"],
                                   data$value[data$parameter=="pop_3"]*data$value[data$parameter=="inp3"],
                                   data$value[data$parameter=="pop_4"]*data$value[data$parameter=="inp4"],
                                   data$value[data$parameter=="pop_5"]*data$value[data$parameter=="inp5"])*100
#incidence of cases of medical impoverishment among those with inpatient care for rotavirus

#IV-Total costs of the policy to the public sector

policy_costs <- data$value[data$parameter=="n"]*(data$value[data$parameter=="c_dose"]+data$value[data$parameter=="c_sys"])*c(data$value[data$parameter=="pop_1"]*data$value[data$parameter=="cov_1"],
                                                                                                                             data$value[data$parameter=="pop_2"]*data$value[data$parameter=="cov_2"],
                                                                                                                             data$value[data$parameter=="pop_3"]*data$value[data$parameter=="cov_3"],
                                                                                                                             data$value[data$parameter=="pop_4"]*data$value[data$parameter=="cov_4"],
                                                                                                                             data$value[data$parameter=="pop_5"]*data$value[data$parameter=="cov_5"])
#estimating the total policy costs per income quintile, only focusing on vaccine costs 
#since treatment is assumed to be entirely privately financed 
#note: we assumed unit cost of vaccine constant per income quintile

#V-Summarizing the ECEA

#V-1 Dashboard

ecea_dash <- as.data.frame(rbind(round(deaths_averted),round(oop_baseline),round(cata_baseline),round(pov_baseline)))
colnames(ecea_dash) <- c("I","II","III","IV","V")
rownames(ecea_dash) <- c("Deaths averted","Private expenditures averted","CHE cases averted","Poverty cases averted")

#V-2 Graphical display

x <- ((deaths_averted/policy_costs)*1000000)
y <- ((cata_baseline/policy_costs)*1000000)
plot(y~x,main='Health and FRP benefits from UPF of rotavirus vaccine, per $1M spent',
     pch=19,type='b',lty=3,xlim=c(0.9*min(x),max(x)*1.1),ylim=c(0,max(y)*1.1),
     xlab='Deaths averted',ylab='Cases of catastrophic health expenditures averted',cex.main=0.9)




