###Hou Yue inspire me to consider the for() to solve the problem 1.4###
#Problem1
library(tidyr)
library(dplyr)
library(ggplot2)

#Problem1.1
signif <- read.delim(file="signif.txt",header=T)
Sig_Eqs <- as_tibble(signif)

#Problem1.2
total <- Sig_Eqs %>%
           select(COUNTRY,DEATHS) %>%
           group_by(COUNTRY) %>%
           summarise(TOT_DEATHS=sum(DEATHS,na.rm=T))
maxdeath <- arrange(total,desc(TOT_DEATHS))
print(maxdeath[1:10,])

#Problem1.3
eq_peryear <- Sig_Eqs %>%
                select(YEAR,EQ_PRIMARY) %>%
                filter(EQ_PRIMARY>6.0) %>%
                group_by(YEAR) %>%
                summarise(num_of_eq=length(EQ_PRIMARY))
ggplot(eq_peryear,aes(x=YEAR,y=num_of_eq)) +
  geom_line()
  
#Problem1.4
CountEq_LargestEq <- function(country){
  LargestEq <- Sig_Eqs %>%
                 select(COUNTRY,YEAR,MONTH,DAY,EQ_PRIMARY) %>%
                 filter(COUNTRY==country) %>%
                 summarise(Country=COUNTRY[1],
                           eq_of_country=length(EQ_PRIMARY),
                           year=YEAR[which(EQ_PRIMARY==max(EQ_PRIMARY,na.rm=T))],
                           month=MONTH[which(EQ_PRIMARY==max(EQ_PRIMARY,na.rm=T))],
                           day=DAY[which(EQ_PRIMARY==max(EQ_PRIMARY,na.rm=T))])
  return(LargestEq)
}
the_world <- list()
for (country in unique(Sig_Eqs$COUNTRY)){
  the_world$Country <- c(the_world$Country,CountEq_LargestEq(country)$Country)
  the_world$eq_of_country <- c(the_world$eq_of_country,CountEq_LargestEq(country)$eq_of_country)
  the_world$year <- c(the_world$year,CountEq_LargestEq(country)$year)
  the_world$month <- c(the_world$month,CountEq_LargestEq(country)$month)
  the_world$day <- c(the_world$day,CountEq_LargestEq(country)$day)
}
the_world2 <- as_tibble(the_world)
arrange(the_world2,desc(eq_of_country))

