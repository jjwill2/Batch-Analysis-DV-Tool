#####################################################################################################
# Performs Level 1 (automated) QAQC screening on daily max and daily mean temerature data in template file
# 
# 
# Jason Williams, IDEQ Lewiston Regional office; Robert Esquivel, IDEQ State Office
# last update: 3-3-2022
####################################################################################################


####################################################################################################
# SCREENS Daily Max and Daily Mean Temperature DATA-- ***DO NOT MODIFY***
####################################################################################################

# read in data--------------------------------------------------------------------------------------
# assumes specific formatting 

tempdata <-
  read_excel(paste0("./inputs/", filename), sheet = data.tab, col_names = TRUE, 
             col_types = c("text", "date", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text")) %>% # added col_types 7/13/2021
  mutate(Date = as.Date(substr(Date, 0, 10), format = "%Y-%m-%d"))

           
# Check1: flag days where consecutive daily means differ by set threshold---------------------------

check1 <-
  tempdata %>%
  group_by(Site) %>%
  arrange(Site, Date) %>%
  mutate(mean_previous = lag(DailyMean),
         mean_subsequent = lead(DailyMean)) %>%
  mutate(diff_from_previous = DailyMean - mean_previous,
         diff_from_subsequent = DailyMean - mean_subsequent) %>%
  mutate(consecutive_mean_flag = ifelse(abs(diff_from_previous) > consecutive.mean.threshold, "FLAG", 
                                        ifelse(abs(diff_from_subsequent) > consecutive.mean.threshold, "FLAG", NA)),
         check1_ID = paste(Site, Date)) %>%
  filter(consecutive_mean_flag == "FLAG")

# Check2: flag duplicate Site/Date combinations-----------------------------------------------------

check2 <-
  tempdata  %>%
  group_by(Site, Date) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  mutate(check2_ID = paste(Site, Date))

# check3: flag days with daily max above specified threshold-------------------------------------------------------------------------

check3 <-
  tempdata %>%
  filter(DailyMax > daily.max.threshold) %>%
  mutate(check3_ID = paste(Site, Date))

# check 4: flag days where daily max differs from previous or subsequent daily max by specified threshold-----------------------------------------------------------------

check4 <-
  tempdata %>%
  group_by(Site) %>%
  arrange(Site, Date) %>%
  mutate(max_previous = lag(DailyMax),
         max_subsequent = lead(DailyMax)) %>%
  mutate(diff_from_previous = DailyMax - max_previous,
         diff_from_subsequent = DailyMax - max_subsequent) %>%
  mutate(consecutive_max_flag = ifelse(abs(diff_from_previous) > consecutive.max.threshold, "FLAG", 
                                       ifelse(abs(diff_from_subsequent) > consecutive.max.threshold, "FLAG", NA)),
         check4_ID = paste(Site, Date)) %>%
  filter(consecutive_max_flag == "FLAG")

# check 5: flag days where daily means in OCtober-May are above specified threshold-----------------------------------------------------------------

check5 <-
  tempdata %>%
  mutate(month = month(Date)) %>%
  filter(month %in% c("10", "11", "12", "1", "2", "3", "4", "5")) %>%
  mutate(oct_to_may_mean_flag = ifelse(DailyMean > oct.to.may.mean.threshold, "FLAG", NA)) %>%
  filter(oct_to_may_mean_flag == "FLAG") %>%
  mutate(check5_ID = paste(Site, Date))


# check 6: flag days where daily range exceeds specified threshold------------------------------------

check6 <-
  tempdata %>%
  mutate(dailyrange = DailyMax - DailyMin) %>%
  mutate(dailyrange_flag = ifelse(dailyrange > daily.range.threshold, "FLAG", NA)) %>%
  filter(dailyrange_flag == "FLAG") %>%
  mutate(check6_ID = paste(Site, Date))


# check 7: flag days where observation count is less than specified threshold-------------------------

check7 <-
  tempdata %>%
  mutate(daily_obs_flag = ifelse(N < minimum.daily.obs.threshold, "FLAG", "NA")) %>%
  filter(daily_obs_flag == "FLAG") %>%
  mutate(check7_ID = paste(Site, Date))

# check 8: flag days where # of daily observations is not available-----------------------------------

check8 <-
  tempdata %>%
  mutate(n_count_flag = ifelse(require.Nobs == "Y" & is.na(N), "FLAG", "NA")) %>%
  filter(n_count_flag == "FLAG") %>%
  mutate(check8_ID = paste(Site, Date))

# check 9: flag days marked for manual exclusion------------------------------------------------------

check9 <-
  tempdata %>%
  mutate(manual_exclude_flag = ifelse(is.na(`Manual Exclude`), NA, "FLAG")) %>%
  filter(manual_exclude_flag == "FLAG") %>%
  mutate(check9_ID = paste(Site, Date))

# screened dataset------------------------------------------------------------------------------------

# if fails a check, flag/exclude entire day
tempdata_screened <-
  tempdata %>%
  mutate(consecutive_mean_flag = ifelse(paste(Site, Date) %in% check1$check1_ID, "FLAG", NA),
         duplicate_flag = ifelse(paste(Site, Date) %in% check2$check2_ID, "FLAG", NA),
         daily_max_flag = ifelse(paste(Site, Date) %in% check3$check3_ID, "FLAG", NA),
         consecutive_max_flag = ifelse(paste(Site, Date) %in% check4$check4_ID, "FLAG", NA),
         oct_to_may_mean_flag = ifelse(paste(Site, Date) %in% check5$check5_ID, "FLAG", NA),
         daily_range_flag = ifelse(paste(Site, Date) %in% check6$check6_ID, "FLAG", NA),
         daily_obs_flag = ifelse(paste(Site, Date) %in% check7$check7_ID, "FLAG", NA),
         obs_count_flag = ifelse(paste(Site, Date) %in% check8$check8_ID, "FLAG", NA),
         manual_exclude_flag = ifelse(paste(Site, Date) %in% check9$check9_ID, "FLAG", NA)) %>%
  mutate(exclude = ifelse(consecutive_mean_flag == "FLAG" | duplicate_flag == "FLAG" | 
          daily_max_flag == "FLAG" | oct_to_may_mean_flag == "FLAG" | consecutive_max_flag == "FLAG" | 
          daily_range_flag == "FLAG" | daily_obs_flag == "FLAG" | obs_count_flag == "FLAG" | 
          manual_exclude_flag == "FLAG", "exclude", NA))

exclusions <-
  tempdata_screened %>%
  filter(!is.na(exclude)) %>%
  mutate(exclusion_ID = paste(Site, Date))
