library(readxl)
Inbound_911 <- read_excel
  ("C:/Users/tomse/OneDrive/Desktop/Data Viz/Inbound_911.xlsx")
View(Inbound_911)
emergency <- Inbound_911
summary(emergency)
class(emergency$Call_Date_Time)
emergency$Call_Date <- format(emergency$Call_Date_Time, '%Y-%m-%d')
head(emergency, n = 5)
View(emergency)
emergency$Call_Time <- format(emergency$Call_Date_Time, format = '%H')
as.data.frame(emergency)

class(emergency$Duration_Secs)
as.Date(emergency$Call_Date)

emergency_yr <- emergency%>%
  filter(emergency$Call_Date > '2021-12-31')
View(emergency_yr)
nrow(emergency_yr)

emergency_yr1 <- emergency_yr%>%
  filter(emergency_yr$Duration_Secs > 0)

nrow(emergency_yr1)

emergency_yr1$Call_Time<- as.numeric(emergency_yr1$Call_Time)

emergency_yr1$time_of_day[emergency_yr1$Call_Time >= 0 & 
                           emergency_yr1$Call_Time <= 3]<-'12am-3am'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 3 & 
                           emergency_yr1$Call_Time <= 6]<-'3am-6am'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 6 & 
                           emergency_yr1$Call_Time <= 9] <-'6am-9am'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 9 & 
                            emergency_yr1$Call_Time <= 12]<-'9am-12pm'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 12 & 
                            emergency_yr1$Call_Time <= 15]<-'12pm-3pm'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 15 & 
                            emergency_yr1$Call_Time <= 18]<-'3pm-6pm'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 18 & 
                            emergency_yr1$Call_Time <= 21]<-'6pm-9pm'
emergency_yr1$time_of_day[emergency_yr1$Call_Time > 21 & 
                            emergency_yr1$Call_Time <= 23]<-'9pm-12am'
emergency_yr2 <- emergency_yr1


emergency_yr2$time_of_day<-factor(emergency_yr2$time_of_day,
                                  levels=c('12am-3am','3am-6am', '6am-9am',
                                           '9am-12pm','12pm-3pm','3pm-6pm',
                                          '6pm-9pm', '9pm-12am'))
View(emergency_yr2)
as.data.frame(emergency_yr2) 

ggplot(data = emergency_yr2, aes(x = emergency_yr2$time_of_day, 
                                 fill =emergency_yr2$time_of_day))+ 
  geom_bar(stat = "count")+
  scale_fill_manual(values = c("12am-3am" = "gray38",
                               "3am-6am" = "gray38",
                               "6am-9am" = "gray38",
                               "9am-12pm" = "red",
                               "12pm-3pm" = "red",
                               "3pm-6pm" = "red",
                               "6pm-9pm" = "gray38",
                               "9pm-12am" = "gray38")) +
  ggtitle("911 Calls Peak between 9am and 6pm")+
  scale_y_continuous("Number of Calls\n(In 2022)", breaks = seq(0, 120000, by=10000))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_blank(),plot.title=element_text(hjust=0.5),
        legend.position="none")

avg_time <- as.data.frame(table(emergency_yr2$time_of_day))
avg_time
mean(avg_time$Freq)

emergency_yr2$Call_Month <- format(as.Date(emergency_yr2$Call_Date, '%Y-%m-%d'),'%m')

emergency_yr2$Month_sp[emergency_yr2$Call_Month == '01']<-'Jan'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '02']<-'Feb'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '03']<-'Mar'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '04']<-'Apr'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '05']<-'May'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '06']<-'Jun'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '07']<-'Jul'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '08']<-'Aug'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '09']<-'Sep'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '10']<-'Oct'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '11']<-'Nov'
emergency_yr2$Month_sp[emergency_yr2$Call_Month == '12']<-'Dec'

calls_by_month<- as.data.frame(table(emergency_yr2$Month_sp))   
colnames(calls_by_month)<-c('Months_Abv', 'Count_of_Calls')
calls_by_month
mean(calls_by_month$Count_of_Calls)
class(calls_by_month$Months_Abv)

ggplot(data=calls_by_month, aes(x=Months_Abv, y=Count_of_Calls, group=1)) +
  geom_line(color='red')+
  scale_x_discrete(name = "Months (2022)", limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous("Count of Calls", limits = c(0, 90000, by=10000))+
  labs(title="January is the Busiest Month")+
  
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5),panel.border = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


emergency_yr2$day_of_week <-weekdays(emergency_yr2$Call_Date_Time, 
                                     abbr = TRUE)

ggplot(data = emergency_yr2, aes(x = emergency_yr2$day_of_week, 
                                 fill =emergency_yr2$day_of_week))+
  geom_bar(stat = "count")+
  scale_fill_manual(values = c("Sun" = "gray38",
                               "Mon" = "red",
                               "Tue" = "gray38",
                               "Wed" = "gray38",
                               "Thu" = "gray38",
                               "Fri" = "red",
                               "Sat" = "gray38")) +
  ggtitle("911 Calls at Highest on Friday and Monday")+
  scale_y_continuous("Number of Calls\n(In 2022)", breaks = seq(0, 100000, by=10000))+
  scale_x_discrete(name = "Months (2022)", limits = c("Sun", "Mon", "Tue", "Wed", "Thu",
                                                      "Fri", "Sat"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_blank(),plot.title=element_text(hjust=0.5),
        legend.position="none")

calls_by_day<- as.data.frame(table(emergency_yr2$day_of_week))
calls_by_day

mean(calls_by_day$Freq)
nrow(emergency_yr2)
summary(emergency_yr2)
