#####################################################################################################
# Batch compares to dialy values to Aquaatic Life temperature criteria
# exports results to excel, temp plots to .pdf file
# 
# Jason Williams, IDEQ Lewiston Regional office; Robert Esquivel, IDEQ State Office
# last update: 3-4-2022
####################################################################################################


####################################################################################################
 # Loads Required R packages so script is reproducible -- ***DO NOT MODIFY***
####################################################################################################

# uses renv for package dependencies
library(renv)

renv::restore()

# loads required R packages from renv cache
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(writexl)
library(lubridate)
library(zoo)


####################################################################################################
# RUNS SOURCE SCRIPTS ***DO NOT MODIFY****
####################################################################################################

source("user_input_for_batch_analysis_DV_v1.5.R")
source("screens_temp_DV_data_v1.5.R")

####################################################################################################
# ANSLYSIS & OUTPUT -- ***DO NOT MODIFY***
####################################################################################################

# read in data--------------------------------------------------------------------------------------
# assumes specific formatting in excel 

criteria <-
  read_excel(paste0("./inputs/", filename), sheet = criteria.tab, skip = 2, col_names = TRUE, 
             col_types = c("text", "date", "date", "date", "date", "date", "date", 
                           "date", "date", "date", "date", "date", "date", "date", "date"))

# daily statistics----------------------------------------------------------------------------------

# daily statistics----------------------------------------------------------------------------------

dailystats <-
  tempdata_screened %>%
  group_by(Site) %>%
  arrange(Site, Date) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  mutate(year = year(Date))

# identify site/year combos with no data-----------------------------------------------------------
# will filter these out of summaries and plots

no_data_years <-
  dailystats %>%
  group_by(Site, year) %>%
  summarise(obs_count = n(),
            non_na_count = sum(!is.na(DailyMax)),
            na_count = sum(is.na(DailyMax))) %>%
  mutate(all_na = ifelse(obs_count == na_count, "Y", "N")) %>%
  filter(all_na == "Y") %>%
  mutate(no_data_id = paste(Site, year))


# Weekly Maximum Temp (WMT)-----------------------------------------------------------------------

WMT <-
  dailystats %>%
  filter(is.na(exclude)) %>% 
  mutate(daynum = yday(Date)) %>%
  mutate(weekstart = daynum, weekend = daynum + 6) %>%
  filter(is.na(exclude)) %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  mutate(WMT = rollapply(DailyMax, 7, mean, align = "right", fill = NA)) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)

# summary by site----------------------------------------------------------------------------------

# general summary - all observations

gen_summary <-
  dailystats %>%
  group_by(Site) %>%
  summarize(`Site Obs Start Date` = min(Date),
            `Site Obs End Date` = max(Date),
            `Site Days` = n_distinct(Date),
            `Site Obs Days` = sum(!is.na(DailyMax)),
            `Site Max` = max(DailyMax, na.rm = TRUE),
            `Site excluded days` = sum(!is.na(exclude)))

# COLD
cold_summary <-
  dailystats %>%
  merge(criteria, by = "Site", all.x = TRUE) %>%
  mutate(daynum = yday(Date), COLD_start_daynum = yday(COLD_start), COLD_end_daynum = yday(COLD_end)) %>%
  mutate(within_COLD_period = ifelse(daynum >= COLD_start_daynum & daynum <= COLD_end_daynum, "yes", "no")) %>%
  filter(within_COLD_period == "yes") %>%
  filter(is.na(exclude)) %>%
  group_by(Site, year) %>%
  summarize(`Site COLD Start` = format(as.Date(max(COLD_start, na.rm = TRUE)), "%m/%d"),
            `Site COLD End` = format(as.Date(max(COLD_end, na.rm = TRUE)), "%m/%d"),
            `Days Evaluated` = sum(!is.na(DailyMax)),
            `COLD Obs Start` = min(Date, na.rm = TRUE),
            `COLD Obs End` = max(Date, na.rm = TRUE),
            `Period Min` = min(DailyMin, na.rm = TRUE),
            `Period Max` = max(DailyMax, na.rm = TRUE),
            `Period Mean` = mean(DailyMean, na.rm = TRUE),
            `Days > COLD max criteria (22 C)` = sum(DailyMax > 22, na.rm = TRUE),
            `Days > COLD daily ave criteria (19 C)` = sum(DailyMean > 19, na.rm = TRUE)) %>%
  merge(gen_summary, by = "Site") %>%
  select(Site,`Site Obs Start Date`, `Site Obs End Date`, `Site Days`, `Site Obs Days`, 
         `Site excluded days`, `Site Max`, 
          year, `Site COLD Start`, `Site COLD End`,  `COLD Obs Start`, `COLD Obs End`, `Days Evaluated`,
         `Period Min`, `Period Max`, `Period Mean`,
         `Days > COLD max criteria (22 C)`, `Days > COLD daily ave criteria (19 C)`) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)


warnings()

# SS
dailystats_ss <-
  dailystats %>%
  merge(criteria, by = "Site", all.x = TRUE) %>%
  mutate(daynum = yday(Date), SS_start_daynum = yday(SS_start), SS_end_daynum = yday(SS_end),
         SS_fall_start_daynum = yday(SS_fall_start), SS_fall_end_daynum = yday(SS_fall_end), 
         SS_spring_start_daynum = yday(SS_spring_start), SS_spring_end_daynum = yday(SS_spring_end)) %>%
  mutate(within_SS_general = ifelse(daynum >= SS_start_daynum & daynum <= SS_end_daynum, "yes", "no"),
         within_SS_fall = ifelse(daynum >= SS_fall_start_daynum & daynum <=SS_fall_end_daynum , "yes", "no"),
         within_SS_spring = ifelse(daynum >= SS_spring_start_daynum & daynum <=SS_spring_end_daynum, "yes", "no"))

# SS general
SS_general <-
  dailystats_ss %>%
  filter(within_SS_general == "yes") %>%
  filter(is.na(exclude)) %>%
  group_by(Site, year) %>%
  summarise(`SS Start` = format(as.Date(max(SS_start, na.rm = TRUE)), "%m/%d"),
            `SS End` = format(as.Date(max(SS_end, na.rm = TRUE)), "%m/%d"),
            `SS Obs Start` = max(SS_start, na.rm = TRUE),
            `SS Obs End` = max(SS_end, na.rm = TRUE),
            `Days Evaluated` = sum(!is.na(DailyMax)),
            `SS Obs Start` = min(Date, na.rm = TRUE),
            `SS Obs End` = max(Date, na.rm = TRUE),
            `Period Min` = min(DailyMin, na.rm = TRUE),
            `Period Max` = max(DailyMax, na.rm = TRUE),
            `Period Mean` = mean(DailyMean, na.rm = TRUE),
            `Days > SS max criteria (13 C)` = sum(DailyMax > 13, na.rm = TRUE),
            `Days > SS daily ave criteria (9 C)` = sum(DailyMean > 9, na.rm = TRUE)) %>%
  merge(gen_summary, by = "Site") %>%
  mutate(period = "SS general") %>%
  select(Site, `Site Obs Start Date`, `Site Obs End Date`, `Site Days`, `Site Obs Days`, 
         `Site excluded days`, `SS Start`, `SS End`,  
         year, period, `SS Obs Start`, `SS Obs End`, `Days Evaluated`,
         `Period Min`, `Period Max`, `Period Mean`,
         `Days > SS max criteria (13 C)`, `Days > SS daily ave criteria (9 C)`) 


# SS spring
SS_spring <-
  dailystats_ss %>%
  filter(within_SS_spring == "yes") %>%
  filter(is.na(exclude)) %>%
  group_by(Site, year) %>%
  summarise(`SS Start` = format(as.Date(max(SS_spring_start, na.rm = TRUE)), "%m/%d"),
            `SS End` = format(as.Date(max(SS_spring_end, na.rm = TRUE)), "%m/%d"),
            `SS Obs Start` = max(SS_spring_start, na.rm = TRUE),
            `SS Obs End` = max(SS_spring_end, na.rm = TRUE),
            `Days Evaluated` = sum(!is.na(DailyMax)),
            `SS Obs Start` = min(Date, na.rm = TRUE),
            `SS Obs End` = max(Date, na.rm = TRUE),
            `Period Min` = min(DailyMin, na.rm = TRUE),
            `Period Max` = max(DailyMax, na.rm = TRUE),
            `Period Mean` = mean(DailyMean, na.rm = TRUE),
            `Days > SS max criteria (13 C)` = sum(DailyMax > 13, na.rm = TRUE),
            `Days > SS daily ave criteria (9 C)` = sum(DailyMean > 9, na.rm = TRUE)) %>%
  merge(gen_summary, by = "Site") %>%
  mutate(period = "SS spring") %>%
  select(Site, `Site Obs Start Date`, `Site Obs End Date`, `Site Days`, `Site Obs Days`, 
         `Site excluded days`,`SS Start`, `SS End`, 
         year, period, `SS Obs Start`, `SS Obs End`, `Days Evaluated`,
         `Period Min`, `Period Max`, `Period Mean`,
         `Days > SS max criteria (13 C)`, `Days > SS daily ave criteria (9 C)`)

# ss fall
SS_fall <-
  dailystats_ss %>%
  filter(within_SS_fall == "yes") %>%
  filter(is.na(exclude)) %>%
  group_by(Site, year) %>%
  summarise(`SS Start` = format(as.Date(max(SS_fall_start, na.rm = TRUE)), "%m/%d"),
            `SS End` = format(as.Date(max(SS_fall_end, na.rm = TRUE)), "%m/%d"),
            `SS Obs Start` = max(SS_fall_start, na.rm = TRUE),
            `SS Obs End` = max(SS_fall_end, na.rm = TRUE),
            `Days Evaluated` = sum(!is.na(DailyMax)),
            `SS Obs Start` = min(Date, na.rm = TRUE),
            `SS Obs End` = max(Date, na.rm = TRUE),
            `Period Min` = min(DailyMin, na.rm = TRUE),
            `Period Max` = max(DailyMax, na.rm = TRUE),
            `Period Mean` = mean(DailyMean, na.rm = TRUE),
            `Days > SS max criteria (13 C)` = sum(DailyMax > 13, na.rm = TRUE),
            `Days > SS daily ave criteria (9 C)` = sum(DailyMean > 9, na.rm = TRUE)) %>%
  merge(gen_summary, by = "Site") %>%
  mutate(period = "SS fall") %>%
  select(Site, `Site Obs Start Date`, `Site Obs End Date`, `Site Days`, `Site Obs Days`, 
         `Site excluded days`, `SS Start`, `SS End`,  
         year, period, `SS Obs Start`, `SS Obs End`, `Days Evaluated`,
         `Period Min`, `Period Max`, `Period Mean`,
         `Days > SS max criteria (13 C)`, `Days > SS daily ave criteria (9 C)`)

ss_summary <-
  rbind(SS_general, SS_spring, SS_fall) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)


# ID Bull Trout Rearing
ID_BT_rearing <-
  WMT %>%
  merge(criteria, by = "Site", all.x = TRUE) %>%
  filter(!is.na(Rearing_start)) %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(Site, year) %>%
  summarize(`BT Rearing Start` = format(as.Date(max(Rearing_start, na.rm = TRUE)), "%m/%d"),
            `BT Rearing End` = format(as.Date(max(Rearing_end, na.rm = TRUE)), "%m/%d"),
            `BT Rearing Obs Start` = min(Date),
            `BT Rearing Obs End` = max(Date),
            `7 Day Periods Evaluated` = sum(!is.na(WMT)),
            `7 Day Periods with WMT > 13 C` = sum(WMT > 13, na.rm = TRUE),
            MWMT = max(WMT, na.rm = TRUE)) %>%
  mutate(criteria = "ID Bull Trout Rearing (13 C MWMT)",
         `MWMT exceeded?` = ifelse(MWMT > 13, "yes", "no"))  %>%
  select(Site, year, criteria, `BT Rearing Start`, `BT Rearing End`, `BT Rearing Obs Start`, `BT Rearing Obs End`, 
         `7 Day Periods Evaluated`, `7 Day Periods with WMT > 13 C`, MWMT, `MWMT exceeded?`) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)

# ID Bull Trout Spawning
ID_BT_spawning <-
  dailystats %>%
  filter(is.na(exclude)) %>% 
  merge(criteria, by = "Site", all.x = TRUE) %>%
  filter(!is.na(Spawning_start)) %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month %in% c(9, 10)) %>%
  group_by(Site, year) %>%
  summarize(`BT Spawning Start` = format(as.Date(max(Spawning_start, na.rm = TRUE)), "%m/%d"),
            `BT Spawning End` = format(as.Date(max(Spawning_end, na.rm = TRUE)), "%m/%d"),
            `BT Spawning Obs Start` = min(Date, na.rm = TRUE),
            `BT Spawning Obs End` = max(Date, na.rm = TRUE),
            `Days Evaluated` = sum(!is.na(DailyMean)),
            `Days criteria exceeded` = sum(DailyMean > 9, na.rm = TRUE)) %>%
  mutate(criteria = "ID Bull Trout Spawning (9 C daily ave)") %>% 
  select(Site, year, criteria, `BT Spawning Start`, `BT Spawning End`,
         `BT Spawning Obs Start`, `BT Spawning Obs End`, `Days Evaluated`, 
         `Days criteria exceeded`) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)

# EPA Bull Trout
EPA_BT <-
  WMT %>%
  merge(criteria, by = "Site", all.x = TRUE) %>%
  filter(!is.na(EPABT_start)) %>%
  mutate(month = month(Date), year = year(Date)) %>%
  filter(month %in% c(6, 7, 8, 9)) %>%
  group_by(Site, year) %>%
  summarize(`EPA BT Start`= format(as.Date(max(EPABT_start, na.rm = TRUE)), "%m/%d"),
            `EPA BT End` = format(as.Date(max(EPABT_end, na.rm = TRUE)), "%m/%d"),
            `EPA BT Obs Start` = min(Date, na.rm = TRUE),
            `EPA BT Obs End` = max(Date, na.rm = TRUE),
            `7 Day Periods Evaluated` = sum(!is.na(WMT)),
            `7 Day Periods with WMT > 10 C` = sum(WMT > 10, na.rm = TRUE),
            MWMT = max(WMT, na.rm = TRUE)) %>%
  mutate(criteria = "EPA Bull Trout Criteria (10 C MWMT)",
         `MWMT exceeded?` = ifelse(MWMT > 10, "yes", "no")) %>%
  select(Site, year, criteria, `EPA BT Start`, `EPA BT End`, `EPA BT Obs Start`, `EPA BT Obs End`, 
         `7 Day Periods Evaluated`, `7 Day Periods with WMT > 10 C`, MWMT, `MWMT exceeded?`) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id)


# output excel file with statistics----------------------------------------------------------------
sheets <-list("user inputs" = user_inputs, "dailystats" = dailystats,  "WMT" = WMT, "COLD summary" = cold_summary,
              "SS summary" = ss_summary, "ID BT rearing" = ID_BT_rearing,
              "ID BT spawning" = ID_BT_spawning, "EPA BT" = EPA_BT)
write_xlsx(sheets, paste0("./outputs/batch_template_metric_analysis_output_", projectname, ".xlsx"))


# output pdf file with all plots---------------------------------------------------------------
site_list <-levels(unique(as.factor(dailystats$Site)))
for_plot <-
  dailystats %>%
  mutate(monthday = format(Date, "%m-%d")) %>%
  mutate(plotdate = as.Date(paste("1901-", monthday, sep = ""), format = "%Y-%m-%d")) %>%
  mutate(`DailyMean Excluded` = ifelse(exclude == "exclude", DailyMean, NA),
         `DailyMax Excluded` = ifelse(exclude == "exclude", DailyMax, NA)) %>%
  filter(!paste(Site, year) %in% no_data_years$no_data_id) %>%
  merge(criteria, by = "Site", all.x = TRUE) %>%
  mutate(daynum = yday(Date), COLD_start_daynum = yday(COLD_start), COLD_end_daynum = yday(COLD_end)) %>%
  mutate(within_COLD_period = ifelse(daynum >= COLD_start_daynum & daynum <= COLD_end_daynum, "yes", "no")) %>%
  mutate(COLD_mean = ifelse(within_COLD_period == "yes", 19, NA), COLD_max = ifelse(within_COLD_period == "yes", 22, NA)) %>%
  mutate(SS_start_daynum = yday(SS_start), SS_end_daynum = yday(SS_end),
         SS_fall_start_daynum = yday(SS_fall_start), SS_fall_end_daynum = yday(SS_fall_end), 
         SS_spring_start_daynum = yday(SS_spring_start), SS_spring_end_daynum = yday(SS_spring_end)) %>%
  mutate(within_SS_general = ifelse(daynum >= SS_start_daynum & daynum <= SS_end_daynum, "yes", "no"),
         within_SS_fall = ifelse(daynum >= SS_fall_start_daynum & daynum <=SS_fall_end_daynum , "yes", "no"),
         within_SS_spring = ifelse(daynum >= SS_spring_start_daynum & daynum <=SS_spring_end_daynum, "yes", "no")) %>%
  mutate(SS_mean = ifelse(within_SS_general == "yes" | within_SS_fall == "yes" | within_SS_spring == "yes", 9, NA)) %>%
  mutate(SS_max = ifelse(within_SS_general == "yes" | within_SS_fall == "yes" | within_SS_spring == "yes", 13, NA))

for_plot$`DailyMean Excluded` <- as.numeric(for_plot$`DailyMean Excluded`)
for_plot$`DailyMax Excluded` <- as.numeric(for_plot$`DailyMax Excluded`)


# substitutes NA for flagged days so data are not plotted
for_plot$DailyMean[!is.na(for_plot$exclude)]<-NA
for_plot$DailyMax[!is.na(for_plot$exclude)]<-NA

#Changes Daily Mean and Daily Max Exluded to a numeric data type if there was no data excluded from the analysis 
for_plot$`DailyMean Excluded` <- as.numeric(for_plot$`DailyMean Excluded`)
for_plot$`DailyMax Excluded` <- as.numeric(for_plot$`DailyMax Excluded`)

pdfpath <-paste0("./outputs/batch_DV_temp_plots_", projectname, ".pdf")
pdf(file = pdfpath, onefile = TRUE)

# make plots
for (i in site_list) {
  
  temp_plot <-
    for_plot %>%
    filter(Site == i) %>%
    ggplot(aes(x = plotdate)) +
    geom_line(aes(y = DailyMean, color = "DailyMean")) +
    geom_line(aes(y = DailyMax, color = "DailyMax")) +
    geom_line(aes(y = `DailyMean Excluded`, color = "excluded mean")) +
    geom_line(aes(y = `DailyMax Excluded`, color = "excluded max")) +
    geom_line(aes(y = COLD_mean), color = "#F8766D", linetype = 'dotted') +
    geom_line(aes(y = COLD_max), color = "#00BFC4", linetype = 'dotted') +
    geom_line(aes(y = SS_mean), color = "#F8766D", linetype = 'dotted') +
    geom_line(aes(y = SS_max), color = "#00BFC4", linetype = 'dotted') +
    #geom_hline(yintercept=22, linetype='dotted', col = "#00BFC4") +
    #geom_hline(yintercept=19, linetype='dotted', col = "#F8766D") +
    #geom_hline(yintercept=13, linetype='dotted', col = "#00BFC4") +
    #geom_hline(yintercept=9, linetype='dotted', col = "#F8766D") +
    scale_color_manual(values = c("DailyMean" = "#F8766D", "DailyMax" = "#00BFC4", 
                                  "COLD_mean" = "#F8766D", "COLD_max" = "#00BFC4",
                                  "excluded mean" = "gray30", "excluded max" = "grey")) +
    facet_wrap(~ year) +
    theme(legend.title = element_blank()) +
    ylab("Temperature C") +
    ggtitle(i) +
    scale_x_date(date_labels = "%b") +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30), minor_breaks=c(9, 13, 19, 22)) +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    theme(legend.title = element_blank()) +
    labs(caption="Note: dashed horizontal lines are CWAL and SS criteria values and user-specified application periods") +
    theme(plot.caption=element_text(size=9, hjust=0, margin=margin(15,0,0,0)))
  print(temp_plot)
  
} # end loop

dev.off()
warnings()
