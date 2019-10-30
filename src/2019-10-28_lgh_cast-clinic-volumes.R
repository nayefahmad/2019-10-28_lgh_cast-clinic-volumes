

#'--- 
#' title: "LGH Cast Clinic volumes"
#' author: "Nayef Ahmad"
#' date: "2019-10-28"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#'     toc_folding: false
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(lubridate)
library(DT)
library(kableExtra)

setup_denodo()

vw_scheduling <- dplyr::tbl(cnx, dbplyr::in_schema("publish", 
                                                   "scheduling"))


#+ rest 
#' # Data 
#' 
#' What clinics are included? 
#' 

vw_scheduling %>% 
  filter(facility_short_name_at_checkin == "LGH") %>% 
  select(appt_loc_desc) %>% 
  collect %>% 
  count(appt_loc_desc) %>% 
  arrange(desc(n)) %>% 
  
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                "condensed", 
                "responsive"))


              