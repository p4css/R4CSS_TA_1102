library(tidyverse)
library(httr)
library(rvest)

url <- "https://tw.feature.appledaily.com/charity/projlist/1"
html <- url %>% read_html()
html %>% html_nodes("td:nth-child(1)") %>% html_text()
html %>% html_nodes("td:nth-child(2)") %>% html_text()
html %>% html_nodes("td:nth-child(3)") %>% html_text()
html %>% html_nodes("td:nth-child(4)") %>% html_text()
html %>% html_nodes("td:nth-child(5)") %>% html_text()
html %>% html_nodes("td:nth-child(6)") %>% html_text()
html %>% html_nodes("td:nth-child(6)") %>% html_nodes("a") %>% html_attr("href")


table <- html %>% html_table()

df_table <-
  table[[1]] %>% as_tibble() %>% slice(-1) %>% `colnames<-`(c("case_id", "title", "date", "status", "amount", "detail"))

link <- html %>% html_nodes(".artcatdetails") %>% html_attr("href")
link_detail <- html %>% html_nodes(".details") %>% html_attr("href")
df_case_list_tmp <- df_table %>% bind_cols(tibble(link = link, link_detail = link_detail))













p_read_html <- possibly(read_html, otherwise = NULL)

##### 抓案例
df_case_list <- tibble()

for(i in 1:10){
  url <- str_c("https://tw.feature.appledaily.com/charity/projlist/", i)
  html <- url %>% curl::curl(handle = curl::new_handle("useragent" = "Mozilla/5.0")) %>% p_read_html()
  table <- html %>% html_table()
  df_table <-
    table[[1]] %>% as_tibble() %>% slice(-1) %>% `colnames<-`(c("case_id", "title", "date", "status", "amount", "detail"))

  link <- html %>% html_nodes(".artcatdetails") %>% html_attr("href")
  link_detail <- html %>% html_nodes(".details") %>% html_attr("href")
  df_case_list_tmp <- df_table %>% bind_cols(tibble(link = link, link_detail = link_detail))

  df_case_list <- df_case_list %>% bind_rows(df_case_list_tmp)
  print(i)
  Sys.sleep(20)
}

df_case_list
df_case_list %>% distinct(case_id)
df_case_list %>% write_rds("data/AS08/df_case_list.rds")
df_case_list %>% write_csv("data/AS08/df_case_list.csv")

##### 抓捐款清單
df_case_donation <- tibble()
index_now <- 1
index_length <- ceiling(dim(df_case_list)[1]/10)

for(i in index_now:index_length){
  j = i*10-9
  k = j+9
  url = df_case_list %>% select(link_detail) %>% slice(j:k) %>% pull()

  html = url %>% map(function(x){x %>% curl::curl(handle = curl::new_handle("useragent" = "Mozilla/5.0")) %>% p_read_html()}) %>%
    set_names(url) %>% compact()
  html_index <- html %>%
    map(function(x){x %>% html_nodes(".addellis-f") %>% html_text() %>% '['(1)}) %>%
    map_lgl(function(x){!is.na(x)})

  html_f <- html[html_index]

  if(length(html_f)==0) {index_now = index_now + 1;print(str_c("all links are dead: article ",j, " to article ", k));next}

  table_raw <- html_f %>% map(function(x){x %>% html_table()})
  df_case_donation_tmp <- table_raw %>% map(function(x){x %>% '[['(2) %>% slice(-1)}) %>% bind_rows(.id = "link_detail") %>% as_tibble() %>%
    `colnames<-`(c("link_detail","donator_order", "donator", "donation", "donate_date"))

  df_case_donation <- df_case_donation %>% bind_rows(df_case_donation_tmp)

  print(str_c("finished article ", j, " to article ", k))
  index_now = index_now + 1
  Sys.sleep(20)
  closeAllConnections()
  gc()
}
