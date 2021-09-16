---
title: "Homework 1"
author: Robert George
date: 9/15/2021
output: github_document
---


library(rvest)
library(tidyverse)
library(xml2)

#read in the website
html = read_html("https://guide.wisc.edu/faculty/")%>%
  html_nodes(".uw-people")

#initialize an empty dataframe
empty = data.frame(name = "",
                   position = "",
                   department = "",
                   degree = "")

#iterate through letters of the alphabet
for (i in 1:26){
  a_html = html[i]%>%
    html_nodes("p")
  a = as_list(a_html)
  #iterate through entries within letter
  for (j in 1:length(a)){
    b = unlist(a[j])
    #drop entries missing an element
    if(length(b) == 4){
      df = as_data_frame(b)
      df["col"] = c("name", "position", "department", "degree")
      df = pivot_wider(df, values_from = value, names_from = col)
      empty = rbind(empty, df)
    }
  }
}

#resulting dataset drops first empty row
result = empty[-1,]

head(result)
