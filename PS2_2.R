###Li Yuan explained to me what is Wind Speed in problem set 2 and tell me I can use the ggsave()###


library("tidyr")
library("dplyr")
library("ggplot2")

#Problem2
plot_monthly_avg_ws <- function(){
  data_tibble <- as_tibble(read.csv("2281305.csv",header = TRUE))
  
  #get wind rate data
  data_tibble_monthly_wr <- data_tibble %>% mutate(Year=as.Date(substr(DATE,1,4),"%Y"),
                                                   Month=as.factor(substr(DATE,6,7)),
                                                   Wind_Rate_Origin=substr(WND,9,12),
                                                   Wind_Rate=as.numeric(Wind_Rate_Origin)*0.1,
                                                   Wind_Flag=substr(WND,14,14)) %>% 
    #clean the data
    filter(Wind_Rate_Origin!=9999 & Wind_Flag==1) %>%
    select(Year,Month,Wind_Rate) %>% 
    group_by(Year,Month)  %>% 
    summarize(Monthly_Wind_Rate=mean(Wind_Rate,na.rm = TRUE))  %>%
    ggplot(aes(x=Year, y=Monthly_Wind_Rate, color=Month)) + 
    scale_x_date(breaks="3 years",date_labels="%Y")+
    geom_line() +
    facet_wrap(~ Month)
  ggsave("Shenzhen_Wnd_LiuYiwen.png",data_tibble_monthly_wr,height = 9,width = 15,units = "cm")
}


#plot it
plot_monthly_avg_ws()


