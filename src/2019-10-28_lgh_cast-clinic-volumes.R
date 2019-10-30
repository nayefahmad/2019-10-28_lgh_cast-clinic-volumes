

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
  
  datatable(extensions = 'Buttons',
          options = list(dom = 'Bfrtip', 
                         buttons = c('excel', "csv")))
                         

#' Pull Cast Clinic data only: 
#' 

df1.cast_clinic <- 
  vw_scheduling %>% 
  filter(appt_loc_desc == "LGH Cast Clinic", 
         appt_start_date_id <= "20191029") %>% 
  select(encntr_id, 
         encntr_type_desc_at_checkin, 
         encntr_type_class_desc_at_checkin, 
         appt_start_date_id,
         is_appt_checkin, 
         is_appt_cancel, 
         is_appt_no_show, 
         appt_loc_desc,
         appt_type_desc) %>% 
  collect() %>% 
  mutate(appt_start_date = ymd(appt_start_date_id), 
         weekday = weekdays(appt_start_date) %>% factor(levels = c("Monday", 
                                                                   "Tuesday", 
                                                                   "Wednesday", 
                                                                   "Thursday", 
                                                                   "Friday", 
                                                                   "Saturday", 
                                                                   "Sunday")))

# str(df1.cast_clinic)
# summary(df1.cast_clinic)

#' Most common procedures: 
#' 

df1.cast_clinic %>% 
  count(appt_type_desc) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                "condensed", 
                "responsive"))
              

#' Group by day: 
#' 
df2.group <- 
  df1.cast_clinic %>% 
  count(appt_start_date, 
        weekday) %>% 
  arrange(appt_start_date) %>% 
  fill_dates(date_col = appt_start_date,
             start_date = "2018-04-30", 
             end_date = "2019-10-29") %>% 
  replace_na(replace = list(n = 0))
  

df2.group %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           
#' # Plots 
#' 

df3.group_no_weekend <- 
  df2.group %>% 
  filter(weekday %in% c("Monday", 
                        "Tuesday", 
                        "Wednesday", 
                        "Thursday", 
                        "Friday")) 
# plot: 
df3.group_no_weekend %>% 
  ggplot(aes(x = dates_fill, 
             y = n)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth() + 
  
  labs(title = "LGH Cast Clinic", 
       subtitle = "Num daily appointments, from CST go-live onwards (excl. weekends)") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))



df3.group_no_weekend %>% 
  ggplot(aes(x = n)) + 
  geom_density()


df3.group_no_weekend %>% 
  ggplot(aes(x = weekday, 
             y = n)) + 
  geom_boxplot() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
  
  


  


  
  
      

