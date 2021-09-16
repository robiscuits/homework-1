Homework 1
================
Robert George
9/15/2021

``` r
library(rvest)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter()         masks stats::filter()
    ## x readr::guess_encoding() masks rvest::guess_encoding()
    ## x dplyr::lag()            masks stats::lag()

``` r
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
```

    ## Warning: `as_data_frame()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
#resulting dataset drops first empty row
result = empty[-1,]

head(result)
```

    ##                   name              position              department
    ## 2      ABBOTT,DAVID H.             Professor Obstetrics & Gynecology
    ## 3   ABD-ELSAYED,ALAA A Assoc Professor (Chs)          Anesthesiology
    ## 4     ABDUALLAH,FAISAL             Professor                     Art
    ## 5 ABRAHAM,OLUFUNMILOLA   Assistant Professor                Pharmacy
    ## 6      ABRAMS,SAMANTHA        Assoc Lecturer      Information School
    ## 7         ABRAMSON,LYN             Professor              Psychology
    ##                                degree
    ## 2    PHD 1979 University of Edinburgh
    ## 3        MD 2000 University of Assiut
    ## 4       PHD 2012 Royal College of Art
    ## 5  PHD 2013 Univ of Wisconsin-Madison
    ## 6   MA 2017 Univ of Wisconsin-Madison
    ## 7 PHD 1978 University of Pennsylvania
