# 若未安裝tidyverse套件，請先安裝
# install.packages("tidyverse")
# 載入套件
library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggplot2)

# 環保署每次最多只能讀入1000筆資料
# 嘗試讀取1000筆AQI資料
query <- "http://opendata.epa.gov.tw/webapi/Data/ATM00679/?$orderby=MonitorDate%20desc&$skip=0&$top=1000&format=json"
test_df <- fromJSON(query)

df <- data.frame()
i <- 0
continue <- TRUE
df1 <- data.frame()
df2 <- data.frame()
while (continue) { # 
  print(paste("from", i+1, "to", i+1000))
  query <- paste0("http://opendata.epa.gov.tw/webapi/Data/ATM00679/?$orderby=MonitorDate%20desc&$skip=", i, "&$top=1000&format=json")
  df1 <- fromJSON(query)
  df <- bind_rows(df, df1) # 將每次讀入的資料合併
  if (setequal(df1, df2)) continue = FALSE # 測試是否有讀入新資料
  else {
    df2 <- df1
    i <- i + 1000
  }
}

# 變更資料型態
df <- df %>%
  mutate(SiteName=as.factor(SiteName)) %>%
  mutate(AQI=as.numeric(AQI))

df <- df %>%
  mutate(MonitorDate=ymd(MonitorDate))

# 選取欄位資料
df <- df %>%
  select(SiteName, MonitorDate, AQI)

# 取出AQI不是NA的資料
df <- df %>%
  filter(!is.na(AQI))

# 加入AQI狀態燈號
df <- df %>%
  mutate(AQIStatus=cut(AQI, c(-Inf, 50, 100, 150, 200, 300, Inf),
                       c("green", "yellow", "orange", "red", "purple", "maroon"),
                       ordered_result=TRUE))

# 查看所有重複的資料
df %>%
  filter(duplicated(.))

# 保留不重複的資料
df <- df %>%
  filter(!duplicated(.))

# 查看每個月的日期數
df %>%
  distinct(MonitorDate) %>%
  mutate(Year=year(MonitorDate), Month=month(MonitorDate)) %>%
  count(Year, Month) %>%
  print(n=nrow(.)) # 列印出所有資料

# 查看2017年7月的資料
df %>%
  filter(year(MonitorDate)==2017 & month(MonitorDate)==7) %>%
  count(MonitorDate) %>%
  print(n = nrow(.))
  

# 查看哪些監測站有缺值？
site_df <- df %>%
  count(SiteName)

site_df %>%
  pull(n) %>%
  table()

# 查看哪些監測站有缺值？
site_df %>%
  filter(n<max(n)) %>%
  arrange(n) %>%
  print(n=nrow(.))

# 取出2017年5月到2018年11月間的資料
df <- df %>%
  filter(MonitorDate>=ymd("2017-05-01") & MonitorDate<=ymd("2018-11-30"))

# 單一變數本身的變化情形
df %>%
  ggplot(aes(x=AQI)) +
  geom_histogram(breaks=seq(0, 300, 25)) +
  scale_x_continuous(breaks=seq(0, 300, 25)) +
  scale_y_continuous(breaks=seq(0, 20000, 2000)) +
  labs(y="資料個數")

# 兩個變數之間的相互變化
# 各監測站測得對健康有影響的天數
df %>%
  mutate(days=n_distinct(MonitorDate)) %>%
  filter(AQIStatus>="orange") %>%
  group_by(days, SiteName) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(percent=round(n*100/days, 2)) %>%
  arrange(desc(n)) %>%
  print(n=nrow(.))
  
## 每天中測得對健康有影響的監測站數
df %>%
  filter(AQIStatus>="orange") %>%
  count(MonitorDate) %>%
  ggplot(aes(x=MonitorDate, y=n)) +
  geom_line() +
  scale_x_date(date_breaks="1 month", minor_breaks = NULL) +
  labs(x="監測日期", y="監測站數") +
  theme(axis.text.x=element_text(angle = 60, hjust=1))

# 選出某一個監測站，畫出AQI變化情形
df %>%
  filter(SiteName=="淡水") %>%
  ggplot(aes(x=MonitorDate, y=AQI)) +
  geom_line() +
  scale_x_date(date_breaks="1 month",  minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(0, 250, 25), limits=c(0, 250), minor_breaks = NULL) +
  labs(x="監測日期") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

df %>%
  filter(SiteName=="淡水") %>%
  ggplot(aes(x=MonitorDate, y=AQI)) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=50, ymin=0), fill="green", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=100, ymin=50), fill="yellow", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=150, ymin=100), fill="orangered", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=200, ymin=150), fill="red", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=250, ymin=200), fill="purple", alpha=0.01) +
  geom_line() +
  scale_x_date(date_breaks="1 month",  minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(0, 250, 25), limits=c(0, 250), minor_breaks = NULL) +
  labs(x="監測日期") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

# 選出某三個監測站，畫出每天的AQI變化情形
df %>%
  filter(SiteName %in% c("淡水", "麥寮", "楠梓")) %>%
  ggplot(aes(x=MonitorDate, y=AQI)) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=50, ymin=0), fill="green", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=100, ymin=50), fill="yellow", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=150, ymin=100), fill="orangered", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=200, ymin=150), fill="red", alpha=0.01) +
  geom_rect(aes(xmax=max(df$MonitorDate), xmin=min(df$MonitorDate), ymax=250, ymin=200), fill="purple", alpha=0.01) +
  geom_line() +
  scale_x_date(date_breaks="1 month",  minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(0, 250, 50), limits=c(0, 250), minor_breaks = NULL) +
  labs(x="監測日期") +
  facet_wrap(~SiteName, nrow=3) +
  theme(axis.text.x=element_text(angle=60, hjust=1))

# 以日曆畫出今年的AQI變化情形
df %>%
  filter(SiteName=="淡水") %>%
  filter(year(MonitorDate)==2018) %>%
  mutate(mo=month(MonitorDate, label=TRUE)) %>%
  mutate(wod=wday(MonitorDate, label=TRUE)) %>%
  mutate(md=mday(MonitorDate)) %>%
  mutate(wk1=week(MonitorDate-md+1)) %>%
  mutate(wk=week(MonitorDate)-wk1) %>%
  mutate(wk=ifelse(wod=="週日", wk+1, wk)) %>%
  mutate(wk=ifelse(wday(MonitorDate-md+1, label=TRUE)=="週日", wk-1, wk)) %>%
  ggplot(aes(x=wod, y=wk, fill=AQIStatus)) +
  geom_tile() +
  geom_text(aes(label=md), color="black") +
  facet_wrap(~mo, ncol=3) +
  scale_y_reverse() +
  scale_fill_identity() +
  theme(axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank())
