library(openxlsx)
library(tidyverse)
library(data.table)
library(stringr)
options(pillar.print_max = Inf)

f = function(path,date1,date2){ #路徑,date1:要確認的日期,date2:正確日期
  setwd(path)
  file.list = list.files(path,pattern = '*.csv') %>% tail(3)
  df.list = lapply(file.list, read.csv)
  df = bind_rows(df.list)
  df$date = as.Date(df$date)
  df$server_name = df$server_name+1
  
  day_road = df %>% #每天總路數
    group_by(date) %>% 
    summarise(n = length(unique(server_name)))
  road_type = df %>% #每天各路發牌類型
    group_by(date,server_name) %>% 
    summarise(類型 = dealer_type,位置 = remote_type) %>% 
    unique()
  
  days_l = length(day_road$date[which(day_road$date >= date1)] %>% unique()) #天數
  days = day_road$date[which(day_road$date >= date1)] %>% unique() #日期
  mch_cnt_l = df$mch_serial[which(df$date >= date1)] %>% unique() %>% sort() %>% length() #機台數
  mch_cnt = df$mch_serial[which(df$date >= date1)] %>% unique() %>% sort()  #機台序號
  mch_d = df %>% 
    filter(date >= date1) %>% 
    group_by(mch_serial) %>% 
    summarise(l_d = max(date))
  
  correct = road_type %>% 
    filter(date == date2) %>% 
    ungroup() %>% 
    select(-date)
  date_ = road_type$date[which(road_type$date >= date1)] %>% unique() %>% sort()
  
  date_error = data.frame()
  for (i in 1:(date_ %>% length())) {
    type = identical(correct,road_type %>% 
                       filter(date == date_[i]) %>% 
                       ungroup() %>% 
                       select(-date))
    if(type == 'FALSE'){
      date_error[i,1] = date_[i] 
    }
  }
  date_error = drop_na(date_error) #錯誤日期
  error = road_type %>% #錯誤狀態
    filter(date %in% date_error$V1)
  return(list(list(days,days_l),list(mch_cnt,mch_cnt_l,mch_d),correct,date_error,error))
}