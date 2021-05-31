
# SETUP -------------------------------------------------------------------

library(tidyverse)
library(pdftools)
library(janitor)
library(lubridate)


# EXPERIMENT --------------------------------------------------------------

studies <- list.files("data/SDOT-Speed-Studies/",
                      full.names = TRUE)

studies_pdf <- studies %>% 
  keep(~str_detect(.x, "pdf$"))

studies_pdf_lengths <- map_int(studies_pdf, pdf_length)

summary(studies_pdf_lengths)

tibble(pages = studies_pdf_lengths ) %>% count(pages)

# This suggests that I may need to reduce the number
# of indicators reported for each site, as the
# studies conducted by IDAX don't report all of the
# statistics included in the other (unnamed) studies.
# 
# Additionally, it may be worthwhile to extract both 
# northbound and southbound data, as traffic speed
# can vary significantly depending on direction.

# CREATE THE PDFTOOLS FUNCTION -------------------------------------------

extract_speed_study <- function(filepath){
    
  if(!str_detect(filepath,".pdf$")){stop("This file is not a pdf.")}
  
    pdf_n_pages <- pdf_info(filepath) %>% 
      pluck("pages")
  
    txt <- pdf_text(filepath) %>% 
      pluck(pdf_n_pages) %>% 
      str_remove_all("\\\n")
    
    
    ### LOCATION
    
    txt_location <- pdf_text(filepath) %>% 
      pluck(pdf_n_pages) %>% 
      str_split("\\\n") %>% 
      pluck(1) %>% 
      head(3) %>% 
      str_extract(".*?(?=\\s{20,})") %>% 
      str_squish() %>% 
      str_remove("ON STREET : |LOCATED : |CROSS STREEET : ")
    
    tbl_location <- txt_location %>% 
      matrix(ncol = 3) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      suppressWarnings() %>% 
      `colnames<-`(c("street","xstreet_dir","xstreet")) %>% 
      mutate(location = str_c(street, xstreet_dir, xstreet, sep = " "))
    
    ### DATE AND TIME
    
    txt_datetime <- pdf_text(filepath) %>% 
      pluck(pdf_n_pages) %>% 
      str_split("\\\n") %>% 
      pluck(1) %>% 
      head(3) %>% 
      str_extract("(?<=\\s{20}).*$") %>% 
      str_remove("-") %>% 
      str_remove("Site: ") %>% 
      str_squish() %>% 
      str_remove("ON STREET : |LOCATED : |CROSS STREEET : ")
    
    
    create_datetime <- function(x){strptime(x, "%A, %Om/%d/%Y %I:%M %p")}
    
    tbl_datetime <- txt_datetime %>% 
      matrix(ncol = 3) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      `colnames<-`(c("site","datetime_start","datetime_end")) %>% 
      transmute(
        site,
        datetime_start = create_datetime(datetime_start),
        datetime_end = create_datetime(datetime_end)) %>% 
      mutate(
        date_start = lubridate::as_date(datetime_start),
        date_end = lubridate::as_date(datetime_end),
        time_start = hms::as_hms(datetime_start),
        time_end = hms::as_hms(datetime_end)
      )
    
    ### AVG + MIN + MAX SPEEDS
    
    txt_avg <- str_extract(txt,"(?<=Average\\s\\(Mean\\)\\s).*?(?=\\smph)") %>%  
      as.numeric()
    
    txt_min <- str_extract(txt,"(?<=Minimum\\s).*?(?=\\smph)") %>% 
      as.numeric()
    
    txt_max <- str_extract(txt,"(?<=Maximum\\s).*?(?=\\smph)") %>% 
      as.numeric()
    
    ### SPEED PERCENTILE
    
    txt_pctile <- str_extract(txt,"(?<=Percentile Speeds).*?(?=Speeds Exceeded)") %>% 
      str_remove_all("\\(mph\\)") %>% 
      str_squish() %>% 
      str_split(" ") %>% 
      pluck(1)
    
    tbl_pctile <- tibble(txt_pctile[6:10]) %>% 
      t() %>% 
      `colnames<-`(txt_pctile[1:5]) %>%
      as_tibble() %>% 
      clean_names()
    
    
    ### SPEEDS EXCEEDED
    
    txt_speedexc <- str_extract(txt,"(?<=Speeds Exceeded).*?(?=Study Grand Totals)") %>% 
      str_remove_all("mph") %>%
      str_remove_all("%") %>% 
      str_squish() %>% 
      str_split(" ") %>% 
      pluck(1)
    
    rn_speedexc <- txt_speedexc[1:6]
    
    
    tbl_speedexc <- txt_speedexc[7:length(txt_speedexc)] %>% 
      matrix(nrow = 2) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      `colnames<-`(rn_speedexc) %>% 
      rename_all(.funs = ~str_c("pctile_",.)) %>% 
      gather() %>% 
      mutate(key2 = case_when(
        str_detect(value,"\\(") ~ "count",
        TRUE ~ "pct"
      )) %>% 
      mutate(value = str_remove_all(value,"\\(|\\)")) %>% 
      unite("key",c("key","key2")) %>% 
      pivot_wider(names_from = "key", values_from = "value")
    
    ### TOTAL
    
    tbl_total <- txt %>% 
      str_extract("Combined\\s+\\d+") %>% 
      str_extract("\\d+") %>% 
      as_tibble(.name_repair = "minimal") %>% 
      `colnames<-`("total_vehicles")
    
    
    ### CREATE TIBBLE
    
    tbl_ready <- list(tbl_location, 
                      tbl_datetime,
                      tbl_pctile, 
                      tbl_speedexc,
                      tbl_total) %>% 
      map_dfc(c)
    
    return(tbl_ready)
}


### TEST

fp1 <- "data/SDOT-Speed-Studies/2019_01_03_10932_SGT.pdf"

fp2 <- "data/SDOT-Speed-Studies/2019_01_03_520079_SGT.pdf"

study1 <- extract_speed_study(fp1)

study2 <- extract_speed_study(fp2)

rbind(study1,study2) %>% View()
