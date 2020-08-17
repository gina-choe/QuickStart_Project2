syst2 <- data.frame(Position = c(0,0,rep(c( 1,0), 3)),
                    Name = rep(c("SYS2", "SYS2","SYS4","SYS4"), each=2),
                    start = c("2018-10-01","2018-10-11","2018-11-26","2018-12-06","2018-10-01","2018-10-24","2018-11-23","2018-12-05"),
                    end = c("2018-10-16","2018-11-26","2018-12-06","2018-12-31","2018-10-24","2018-11-23","2018-12-05","2018-12-31"),
                    color = c('#FF0000','#FF0000',rep(c("#008000",'#FF0000'), 3)))

# make Dates comparable
syst2$start <- as.Date(as.character(syst2$start))
syst2$end <- as.Date(as.character(syst2$end))

# check if ranges of the same type overlap, extend first range if it does
syst2 <- syst2[order(syst2$Name, syst2$start),]
for(row in 1:(nrow(syst2) - 1)) 
  if(syst2$Name[row] == syst2$Name[row+1] & syst2$end[row] > syst2$start[row+1]){
    syst2$end[row] <- syst2$end[row+1]
    syst2$start[row+1] <- syst2$start[row]
  }

# kill the duplicates
syst2 <- unique(syst2)

data_narrow <- data_merge[c("userid", "start_time", "end_time")]
data_slim<- data_narrow[order(data_narrow$userid, data_narrow$start_time),]

library(IRanges)
ir_intervals <- IRanges(as.numeric(data_slim$start_time), as.numeric(data_slim$end_time, name=data_slim$userid))
ir_intersects <- Reduce(intersect, split(ir_intervals, data_slim$userid))
cov <-coverage(ir_intervals)
cov
ir_intersects

slim <- data_slim

library(data.table)
setDT(slim)

slim[, start:=as.numeric(start_time)]
slim[, end:=as.numeric(end_time)]
slim$interval <- interval(slim$end_time, slim$start_time)

setkey(slim, userid, start_time, end_time)

ovl <- foverlaps(slim, slim, type = "within", nomatch=0, which=TRUE)[, .N, by=.(userid)]
slim
slim$count <- sapply(slim$Slot, function(x) sum(x %within% bookingsInterval))
count <- [, .N, by=yid]


my_df[,NUMBER_OF_TASKS:=foverlaps(my_df,my_df,which=TRUE)[,.N,by=id]$N]
my_df
!duplicated(data1),]
library(IRanges)
ir_intervals 
slim$overlap <- lapply(split(IRanges(as.vector(slim$start), as.vector(slim$end), names = slim$userid), slim$userid), overlapsAny)
lapply(split(IRanges(as.vector(df$Start), as.vector(df$End), names = df$Students), df$Language), overlapsAny, ignoreSelf = TRUE)
vistime(syst2, events = "Position", groups = "Name")

#######################################################


data <- read.csv('Project2Data.csv')

cols <- c("userid", "attemptresult", "type", "maxsessionuid")
data <- data %>% mutate_at(cols, funs(as.factor(.)))

## 1. How many unique users? A simple number will do here, no tables or formatting needed.
length(unique(data$userid))

## 2. How many unique levels, unique users in each? I want a table that shows this information together.
data_user <- unique(data[ ,c('userid','type')])
data_user %>% group_by(type) %>% summarize(num_users = n())

## 3. What is the average session time for each unique group?
data$attemptdate <- as_datetime(data$attemptdate)
data_clean <- data %>% filter(!is.na(data$maxsessionuid) & data$maxsessionuid != 0)

data_clean %<>% group_by(maxsessionuid) %>% mutate(start = min(attemptdate), end = max(attemptdate))
data_clean$interval <- data_clean$end -data_clean$start

data_interval <- data_clean %>% select(userid, type, maxsessionuid, interval) 
%>% distinct() %>% filter(interval > 0)
data_interval_trimmed <- data_interval %>% filter(interval > 0)
data_interval %>% group_by(type) %>% summarize(average = mean(interval))

data_user <- unique(data[ ,c('userid','type')])
data_type <- data_user %>% group_by(type) %>% count(type)

## 3. What is the average session time for each unique group?
data$attemptdate <- as_datetime(data$attemptdate)
data_clean <- data %>% filter(!is.na(maxsessionuid) & maxsessionuid != 0)
data_nosys <- data_clean %>% filter(attemptresult != "SYSLOGOUT")

data_clean %<>% group_by(maxsessionuid) %>% mutate(start = min(attemptdate), end = max(attemptdate))
data_clean$interval <- data_clean$end -data_clean$start
data_clean$interval <- difftime(data4$end, data4$start, units = "mins")

data_interval <- data_clean %>% select(userid, type, maxsessionuid, interval) 
%>% distinct() %>% filter(interval > 0)
data_interval_trimmed <- data_interval %>% filter(interal > 0)
data_interval %>% group_by(type) %>% summarize(average = mean(interval))

data4 <- data_clean %>% group_by(maxsessionuid) %>% mutate(start = min(attemptdate), end = max(attemptdate))
data4$interval <- difftime(data4$end, data4$start, units = "mins")

data4_interval <- data4 %>% select(userid, type, maxsessionuid, interval) %>% distinct() %>% filter(interval > 0)
data4_interval %>% group_by(userid) %>% summarize(n)

data5 <- data4 %>% group_by(maxsessionuid) %>% mutate(start_result = if_else(attemptdate == start, as.character(attemptresult), NULL),
                                                      end_result = if_else(attemptdate == end, as.character(attemptresult), NULL))
data6 <- data5 %>% select(c("maxsessionuid","start_result","end_result"))

data7 <- aggregate(.~ maxsessionuid, data=data6, FUN=na.omit, na.action="na.pass")
data7 <- aggregate(data6[c("start_result","end_result")], data6["maxsessionuid"], FUN=na.omit)

setDT(ovl)
ovl <- slim[, 1:3]
ovl[ ,START:=as.numeric(start_time)]
ovl[ ,END:=as.numeric(end_time)]

setkey(ovl, userid, START, END)
ovl[ , NUMBER:=foverlaps(ovl, ovl, which=TRUE)[,.N, by=xid]$N]
ovl

ovl %>% group_by(userid) %>% summarize(max_concurrent_sessions = max(NUMBER))
ovl_multiple <- ovl %>% filter(max_concurrent_sessions > 1)
count(ovl_multiple)

data_log[data_log$start_time > data_log$end_time, ]
data_dt<- data_log[c("userid", "start_time", "end_time")]
setDT(data_dt)

data_dt[, start:=as.numeric(start_time)]
data_dt[, end:=as.numeric(end_time)]

data_dt[start > end, with = TRUE]

setkey(data_dt, userid, start, end)

data_dt[,NUMBER:=foverlaps(data_dt,data_dt,which=TRUE)[,.N, by=xid]$N]

data_multiple <- data_dt %>% group_by(userid) %>% summarize(max_concurrent_sessions = max(NUMBER))
data_multiple <- data_multiple %>% filter(max_concurrent_sessions > 1)
count(data_multiple)

data_chart <- data_log[c("userid", "start_time", "end_time", "type")]
interval <- seq(min(data_chart$start_time), max(data_chart$end_time), by = "mins")

data_auth <- data_chart[data_chart$type == "AUTHORIZED",]
data_lim <- data_chart[data_chart$type == "LIMITED",]
data_exp <- data_chart[data_chart$type == "EXPRESS",]

n_auth <- sapply(interval, function(int) sum(data_auth$start_time <= int & int <= data_auth$end_time))
n_lim <- sapply(interval, function(int) sum(data_lim$start_time <= int & int <= data_lim$end_time))
n_exp <- sapply(interval, function(int) sum(data_exp$start_time <= int & int <= data_exp$end_time))

library(tidyverse)


n_tot <- sapply(interval, function(int) sum(int %within% data_chart$duration))
n_auth <- sapply(interval, function(int) sum(int %within% data_auth$duration))
n_lim <- sapply(interval, function(int) sum(int %within% data_lim$duration))
n_exp <- sapply(interval, function(int) sum(int %within% data_exp$duration))


max(n_new)
interval_user <- data.frame(interval, n_auth, n_lim, n_exp, n_new)
interval_user1 <- interval_user[1:1000,]

ggplot(interval_user) + 
  geom_line(aes(x=interval, y=n_auth), color="blue", size=0.1)+
  geom_line(aes(x=interval, y=n_lim), color="red", size=0.1) +
  geom_line(aes(x=interval, y=n_exp), color="green", size=0.1) +
  geom_line(aes(x=interval, y=n_new), color="black", size=0.1) +
  labs(title = "Usage", x="time", y="number of users")


data_slim$duration <- interval(data_slim$start_time, data_slim$end_time)

TimeSlot$count <- sapply(TimeSlot$Slot, function(x) sum(x %within% bookingsInterval))

first_result <- min(data_chart$start_time, na.rm=T)
last_result <- max(data_chart$end_time, na.rm=T)
interval1 <- seq.POSIXt( min(data_chart$start_time), max(data_chart$end_time), by = "15 mins")

############################################################################

library(lubridate)
library(tidyverse)

data$attemptdate <- as_datetime(data$attemptdate)
data_clean <- data %>% filter(!is.na(data$maxsessionuid) & data$maxsessionuid != 0)
data_nosys <- data_clean %>% filter(attemptresult != "SYSLOGOUT")

data_nosys <- data_nosys %>% group_by(maxsessionuid) %>% mutate(start = min(attemptdate), end = max(attemptdate))
data_nosys$interval <- difftime(data_nosys$end, data_nosys$start, units = "mins")

data_nosys_trimmed <- data_nosys 
data_nosys_distinct <- data_nosys_trimmed %>% select(userid, type, maxsessionuid, interval) %>% distinct()
data_nosys_distinct %>% group_by(type) %>% summarize(average = mean(interval))

############################################################

library(data.table)
gantt_dt <- as.data.table(gantt)

gantt_dt[gantt_dt, ovl = .foverlaps(gantt_dt, gantt_dt, type = "within", nomatch=0, which=TRUE), 
         Duplicates := .N, by = .EACHI][]
ovl <- foverlaps(gantt_dt, gantt_dt, type = "within", nomatch=0, which=TRUE)[, .N, by=.(userid)]

test <- data_join %>% group_by(maxsessionuid) %>% filter(min(attemptdate))

############################################################

library(tidyverse)
library(lubridate)
install.packages("data.table")
install.packages("ggthemes")
data <- read.csv("Project2Data.csv")

data$attemptdate <- as_datetime(data$attemptdate)
cols <- c("userid", "attemptresult", "type", "maxsessionuid")
data <- data %>% mutate_at(cols, funs(as.factor(.)))
data1 <- data %>% filter(!is.na(data$maxsessionuid) & data$maxsessionuid != 0)
data2 <- data1[!duplicated(data1),]
data1 <- as.tibble(data1)

du2 %>% filter(!userid %in% du1$userid)
# Create matrices of unique users derived from the data before and after cleanup. 
user_original <- matrix(unique(data$userid))
colnames(user_original) <- c("userid")

user_clean <- matrix(unique(data_clean$userid))
colnames(user_clean) <- c("userid")

user_original %>% filter(!userid %in% user_clean$userid)



View(unique(data1$userid))
data_unique <- unique(data$userid)
data1_unique <- as.data.frame(unique(data1$userid))
duplicated(data1_unique)

data1$attemptdate <- as_datetime(data1$attemptdate)
data2 <- data1 %>% pivot_wider(names_from = attemptresult, values_from = attemptdate, values_fn = list(attemptdate = length))

data_login <- data_clean %>% filter(attemptresult == "LOGIN")  %>% dplyr::rename(start_result = attemptresult, start_time = attemptdate)
data_other <-  data_clean %>% filter(attemptresult == "LOGOUT" | attemptresult == "TIMEOUT" | attemptresult == "RESTART" | attemptresult == "SYSLOGOUT")
data_logout <- data_other %>% group_by(maxsessionuid) %>% filter(attemptdate == min(attemptdate))  %>% dplyr::rename(end_result = attemptresult, end_time = attemptdate)

data_merge <- merge(data_login, data_logout, by = c("maxsessionuid", "userid", "type"))

data_merge <- data_merge %>% select(userid, type, maxsessionuid, start_result, end_result, start_time, end_time)
data_merge$interval <- difftime(data_merge$end_time, data_merge$start_time, units = "mins")

data_merge %>% group_by(type) %>% summarize(average = mean(interval))

data_userid <- data_user[order(as.character(data_user$userid)), 1]
gantt <- data_merge[,c("userid","start_time","end_time","type")]
gantt <- gantt[order(gantt$userid, gantt$start_time),]
library(ggthemes)
library(ggplot2)

duplicated(merge_user)

gantt_set1 <- gantt %>% filter(userid %in% data_userid[1:100])
gantt_set1w1 <- gantt_set1 %>% filter(end_time < as_datetime("2020-03-08 00:00:00"))
ggplot(gantt, aes(x=start_time, xend=end_time, y=userid, yend=userid, color=type)) +
  geom_segment() +
  labs(title = "Maximo usage by Users over time", x = "Time", y = "UserID") 

data_merge
distinct_user <- data_merge %>% summarize(n_distinct(userid))
unique_user <- length(unique(data_merge$userid))

colnames(user_clean) <- toupper(names(user_clean))
names(user_clean)

data_dt[, start:=as.numeric(start_time)]
data_dt[, end:=as.numeric(end_time)]

# Set the keys necessary to determine how the aggregation will be executed.
setkey(data_dt, userid, start, end)

# Perform a self-join aggregated by userid to count the number of concurrent sessions during each session. 
data_dt[,num_sessions:=foverlaps(data_dt,data_dt, type="any", which=TRUE)[,.N, by=xid]$N]

# Create a dataframe showing each user's maximum number of concurrent sessions observed.
data_multiple <- data_dt %>% group_by(userid) %>% summarize(max_concurrent_sessions = max(num_sessions))

# Show which users had multiple concurrent sessions at some point during data collection.
data_multiple <- data_multiple %>% filter(max_concurrent_sessions > 1)
data_multiple

############################################################

