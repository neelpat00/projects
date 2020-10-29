data_dir<-"/Users/neel/Desktop/ACTL3141/Assignment"
library('dplyr')
library(splines)
library(ggplot2)
library(viridis)


setwd(data_dir)
#Load life data
life_data<-read.csv("deathspopsimdengland20012018.csv",sep=",",header = TRUE)

#TASK 1
d <- 1 
i <- 0
j <- 1
y <- 2001
deaths_f <- c()
populations_f <-c()
deaths_m <- c()
populations_m <-c()
age <- c(0:90)
breaks <- c(1,seq(4,90, 5))
esp <- c(1000,4000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
death_at_agei_f <- 0
population_at_agei_f <- 0
death_at_agei_m <- 0
population_at_agei_m <- 0
decile_year_dasdr_f <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("year", "decile", "dasdr"))))
decile_year_dasdr_m <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("year", "decile", "dasdr"))))
decile_year_lifeexp_f <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("year", "decile", "life_expectancy"))))
decile_year_lifeexp_m <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("year", "decile", "life_expectancy"))))
ip0_f <- c()
ip0_m <- c()
while(d <= 10){
  while (y <=2018) {
    while(i <= 90){
      if(i<90){
        decile_d_f <- life_data%>%
          filter(life_data$IMD.Decile == d, life_data$age == i, Registration.year == y, sex == "female")
        decile_d_m <- life_data%>%
          filter(life_data$IMD.Decile == d, life_data$age == i, Registration.year == y, sex == "male")
        death_at_agei_f <- death_at_agei_f + sum(decile_d_f$deaths)
        population_at_agei_f <- population_at_agei_f + sum(decile_d_f$population)
        death_at_agei_m <- death_at_agei_m + sum(decile_d_m$deaths)
        population_at_agei_m <- population_at_agei_m + sum(decile_d_m$population)
        
        if(i == 0){
          ip0_f[1] <- (1-decile_d_f$deaths/decile_d_f$population)
        }else{
          ip0_f[i+1] <- ip0_f[i]*(1-decile_d_f$deaths/decile_d_f$population)
        }
        
        if(i == 0){
          ip0_m[1] <- (1-decile_d_m$deaths/decile_d_m$population)
        }else{
          ip0_m[i+1] <- ip0_m[i]*(1-decile_d_m$deaths/decile_d_m$population)
        }
        
        if(i == 0){
          deaths_f <- c(deaths_f, death_at_agei_f)
          populations_f<- c(populations_f, population_at_agei_f)
          death_at_agei_f <- 0
          population_at_agei_f <- 0
          
          deaths_m <- c(deaths_m, death_at_agei_m)
          populations_m<- c(populations_m, population_at_agei_m)
          death_at_agei_m <- 0
          population_at_agei_m <- 0
          j <- j +1
        }else if(i+1>breaks[j]){
          deaths_f <- c(deaths_f, death_at_agei_f)
          populations_f<- c(populations_f, population_at_agei_f)
          death_at_agei_f <- 0
          population_at_agei_f <- 0
          
          deaths_m <- c(deaths_m, death_at_agei_m)
          populations_m<- c(populations_m, population_at_agei_m)
          death_at_agei_m <- 0
          population_at_agei_m <- 0
          j <- j +1
        }
      }else{
        decile_1 <- life_data%>%
          filter(life_data$IMD.Decile == d, life_data$age == "90+", Registration.year == y, sex == "female")
        decile_0 <- life_data%>%
          filter(life_data$IMD.Decile == d, life_data$age == "90+", Registration.year == y, sex == "male")
        
        death_at_agei_f <- sum(decile_1$deaths)
        population_at_agei_f <- sum(decile_1$population)
        death_at_agei_m <- sum(decile_0$deaths)
        population_at_agei_m <- sum(decile_0$population)
        
        deaths_f <- c(deaths_f, death_at_agei_f)
        populations_f<- c(populations_f, population_at_agei_f)
        deaths_m <- c(deaths_m, death_at_agei_m)
        populations_m<- c(populations_m, population_at_agei_m)
        
        death_at_agei_f <- 0
        population_at_agei_f <- 0
        death_at_agei_m <- 0
        population_at_agei_m <- 0
      }
      i <- i +1
    }
    decile_year_dasdr_f[nrow(decile_year_dasdr_f)+1,] <- c(y,d,sum(deaths_f/populations_f * esp))
    decile_year_dasdr_m[nrow(decile_year_dasdr_m)+1,] <- c(y,d,sum(deaths_m/populations_m * esp))
    decile_year_lifeexp_f[nrow(decile_year_lifeexp_f)+1,]<- c(y,d,sum(ip0_f)+4.6)
    decile_year_lifeexp_m[nrow(decile_year_lifeexp_m)+1,]<- c(y,d,sum(ip0_m)+4.1)
    deaths_f <- c()
    populations_f <- c()
    deaths_m <- c()
    populations_m <- c()
    y <- y +1
    i <- 0
    j <- 1
  }
  d <- d +1
  y <- 2001
}

tab <- life_data%>%
  filter(IMD.Decile == 10, age == 0, sex == "female")
tab1 <- life_data%>%
  filter(IMD.Decile == 1, age == 0, sex == "female")
table <- life_data%>%
  filter(IMD.Decile == 10, age == 0, sex == "male")
table1 <- life_data%>%
  filter(IMD.Decile == 1, age == 0, sex == "male")

q0_1_f <- tab1$deaths/tab1$population 
q0_10_f <- tab$deaths/tab$population 
indf_f <-data.frame(qx_1 = q0_1_f, qx_10 = q0_10_f)
q0_1_m <- table1$deaths/table1$population 
q0_10_m <- table$deaths/table$population 
indf_m <-data.frame(qx_1 = q0_1_m, qx_10 = q0_10_m)

ggplot(indf_f, aes(x = c(2001:2018), y = value))+
  geom_line(aes(y = qx_1), col = "blue")+
  geom_line(aes(y = qx_10), col = "red")+
  labs(x = "year", y= "death rate for new born", title = "Infant Mortality for female new borns in Least and Most \n derprived IMD decile",
       colour = "Legend")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(indf_m, aes(x = c(2001:2018), y = value))+
  geom_line(aes(y = qx_1, group = 1), col = "blue")+
  geom_line(aes(y = qx_10, group = 10), col = "red")+
  labs(x = "year", y= "death rate for new born", title = "Infant Mortality for male new borns in Least and Most \n derprived IMD decile",
       colour = "Legend")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="right")



ggplot(decile_year_dasdr_f, aes(x = year, y = dasdr, col = as.factor(decile), group = as.factor(decile))) +
  geom_line(size = 1)+
  scale_fill_hue()+
  labs(x = "year", y = "DASDR", title = "Female DASDR by Decile", col = "Decile")+
  theme(plot.title = element_text(hjust = 0.5)) + ylim(600,2100)

ggplot(decile_year_dasdr_m, aes(x = year, y = dasdr, col = as.factor(decile), group = as.factor(decile))) +
  geom_line(size = 1)+
  scale_fill_hue()+
  labs(x = "year", y = "DASDR", title = "Male DASDR by Decile", col = "Decile")+
  theme(plot.title = element_text(hjust = 0.5))+ ylim(600,2100)

ggplot(decile_year_lifeexp_f, aes(x = year, y = life_expectancy, col = as.factor(decile), group = as.factor(decile))) +
  geom_line(size = 1)+
  scale_fill_hue()+
  labs(x = "year", y = "DASDR", title = "Female Life Expactancy by Decile", col = "Decile")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(decile_year_lifeexp_m, aes(x = year, y = life_expectancy, col = as.factor(decile), group = as.factor(decile))) +
  geom_line(size = 1)+
  scale_fill_hue()+
  labs(x = "year", y = "DASDR", title = "Male life Expactancy by Decile", col = "Decile")+
  theme(plot.title = element_text(hjust = 0.5))

#TASK 2
grad <- life_data%>%
  filter(IMD.Decile == 4, sex == "female", Registration.year == 2018)

x <- c(40:89)
Ex <- grad[41:90,]$population
dx <- grad[41:90,]$deaths
mx <- dx/Ex

#Statistical tests 
chi2Test <- function(O, E, npar, alpha = 0.05){ 
  chi2 <- sum((O - E)^2 / E) #Test statistic
  df <- length(O) - npar
  chi2_alpha <- qchisq(1 - alpha, df) #Critical value
  p.value <- 1 - pchisq(chi2, df) #p.value 
  list(statistic = chi2, c.value = chi2_alpha, df = df, p.value = p.value) 
}

stdTest <- function(zx, breaks = c(-Inf, -1, 0, 1, Inf)){
  observed <- table(cut(zx, breaks)) #count obs ervation in each interval
  expected.p <- diff(pnorm(breaks)) #expected p robabilities for standard normal
  chisq.test(observed, p = expected.p) #apply c hisquare test
}

cumDevTest<- function(A, E, alpha = 0.05){ 
  cumDev <- sum(A - E) / sqrt(sum(E)) #Test statistic
  z_alpha <- qnorm(1 - alpha/2) #Critical value 
  p.value <- 2 *(1 - pnorm(cumDev)) #p.value (Note it is two-tailed)
  list(statistic = cumDev, c.value = z_alpha,p.value = p.value) 
}

groupSignTest <- function(zx, alpha = 0.05){ 
  #Count +'s and -'s
  signs <- sign(zx)
  n1 <- sum(signs == 1)
  n2 <- sum(signs == -1)
  #Count runs
  y <- c(-1, sign(zx))
  G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1]!= -1) # No Runs
  #Normal approximation
  mu <- n1 * (n2 + 1) / (n1 + n2)
  s2 <- (n1 * n2)^2 / (n1 + n2)^3
  G_alpha <- qnorm(alpha, mean = mu, sd = sqrt(s2)) #Critical value
  p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) #p.value (one sided)
  list(statistic = G, c.value = G_alpha, p.value = p.value) 
}

#Gompertz
gompertz <- nls(mx ~ exp(b0 + b1*x), start = list(b0 = 1, b1 = 0), weights = Ex/mx)
gompertz
mx_gompertz <- fitted(gompertz)

#statistical test
zx_gompertz <- (dx-Ex*mx_gompertz)/sqrt(Ex*mx_gompertz)
length(which(zx_gompertz > -2/3 & zx_gompertz < 2/3))
length(zx_gompertz)
chi_gompertz <- chi2Test(dx, Ex*mx_gompertz, 2) #chi-square test
stdTest_gompertz <- stdTest(zx_gompertz) #Standardised deviations test
signTest_gompertz <- binom.test(sum(zx_gompertz > 0), length(x)) #signs test
cumDevTest_gompertz <- cumDevTest(dx,Ex*mx_gompertz) #cumulative distribution test
groupSignTest_gompertz <- groupSignTest(zx_gompertz) #grouping of sign test 

#makeham
makeham <- nls(mx ~ A + exp(b0 + b1*x), start = list(A = 0, b0 = coef(gompertz)[1], b1 = coef (gompertz)[2]), 
               weights = Ex/mx)
makeham
mx_makeham <- fitted(makeham)

#statistical test
zx_makeham <- (dx-Ex*mx_makeham)/sqrt(Ex*mx_makeham)
length(which(zx_makeham > -2/3 & zx_makeham < 2/3))
length(zx_makeham)
chi_makeham <- chi2Test(dx, Ex*mx_makeham, 3) #chi-square test
stdTest_makeham <- stdTest(zx_makeham) #Standardised deviations test
signTest_makeham <- binom.test(sum(zx_makeham > 0), length(x)) #signs test
cumDevTest_makeham <- cumDevTest(dx,Ex*mx_makeham) #cumulative distribution test
groupSignTest_makeham <- groupSignTest(zx_makeham) #grouping of sign test

#Regression Splines
#cubic spline
i <- 2 
knots <- c()
while (i<50) {
  if(log(mx[i]) > log(mx[i+1])){
    knots <- c(knots, i+40.5)
  }
  i <- i + 1
}

cubic_basis <- ns(x, knots = knots)
cubSpline <- lm(mx ~ cubic_basis, weights = Ex/mx )
cubSpline

mx_cubSpline <- fitted(cubSpline)

#statistical tests
zx_cubSpline <- (dx-Ex*mx_cubSpline)/sqrt(Ex*mx_cubSpline)
length(which(zx_cubSpline > -2/3 & zx_cubSpline < 2/3))
length(zx_cubSpline)
chi_cubSpline <- chi2Test(dx, Ex*mx_cubSpline, length(knots)+1) #chi-square test
stdTest_cubSpline <- stdTest(zx_cubSpline) #Standardised deviations test
signTest_cubSpline <- binom.test(sum(zx_cubSpline > 0), length(x)) #signs test
cumDevTest_cubSpline <- cumDevTest(dx,Ex*mx_cubSpline) #cumulative distribution test
groupSignTest_cubSpline <- groupSignTest(zx_cubSpline) #grouping of sign test

#smoothing splines
smSpline <- smooth.spline(x, mx, spar = 0.55) 
smSpline
mx_smSpline <- fitted(smSpline)

#statistical tests
zx_smSpline <- (dx-Ex*mx_smSpline)/sqrt(Ex*mx_smSpline)
length(which(zx_smSpline > -2/3 & zx_smSpline < 2/3))
length(zx_smSpline)
chi_smSpline <- chi2Test(dx, Ex*mx_smSpline, smSpline$df) #chi-square test
stdTest_smSpline <- stdTest(zx_smSpline) #Standardised deviations test
signTest_smSpline <- binom.test(sum(zx_smSpline > 0), length(x)) #signs test
cumDevTest_smSpline <- cumDevTest(dx,Ex*mx_smSpline) #cumulative distribution test
groupSignTest_smSpline <- groupSignTest(zx_smSpline) #grouping of sign test

#plots
df_gompertz <- data.frame(mx,mx_gompertz)
ggplot(df_gompertz, aes(x = c(40:89), y = value))+
  geom_point(aes(y = log(mx)))+
  geom_line(aes(y = log(mx_gompertz), col = "Gompertz"))+
  ylim(-8,0)+
  labs(x = "age", y ="Central rate of mortality (log scale)", color='Legend', 
       title = "Female Gompertz Graduation for IMD decile 4")+
  theme(plot.title = element_text(hjust = 0.5))


df_makeham <- data.frame(mx,mx_makeham)
ggplot(df_makeham, aes(x = c(40:89), y = value))+
  geom_point(aes(y = log(mx)))+
  geom_line(aes(y = log(mx_makeham), col = "Makeham"))+
  ylim(-8,0)+
  labs(x = "age", y ="Central rate of mortality (log scale)", color='Legend', 
       title = "Female Makeham Graduation for IMD decile 4")+
  theme(plot.title = element_text(hjust = 0.5))

df_cubSpline <- data.frame(mx,mx_cubSpline)
ggplot(df_cubSpline, aes(x = c(40:89), y = value))+
  geom_point(aes(y = log(mx)))+
  geom_line(aes(y = log(mx_cubSpline), col = "Cubic Spline"))+
  ylim(-8,0)+
  labs(x = "age", y ="Central rate of mortality (log scale)", color='Legend',
       title = "Female Cubic Splines Graduation for IMD decile 4")+
  theme(plot.title = element_text(hjust = 0.5))


df_smSpline <- data.frame(mx,mx_smSpline)
ggplot(df_smSpline, aes(x = c(40:89), y = value))+
  geom_point(aes(y = log(mx)))+
  geom_line(aes(y = log(mx_smSpline), col = "Smooth Spline"))+
  ylim(-8,0)+
  labs(x = "age", y ="Central rate of mortality (log scale)", color='Legend',
       title = "Female Smoothing Splines Graduation for IMD decile 4")+
  theme(plot.title = element_text(hjust = 0.5))



par(mfrow=c(2,2))
acf(zx_gompertz)
acf(zx_makeham)
acf(zx_cubSpline)
acf(zx_smSpline)
