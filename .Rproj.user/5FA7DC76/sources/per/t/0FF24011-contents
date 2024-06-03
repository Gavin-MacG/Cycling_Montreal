# This script uses both untreated and imputed sensor data and :
# - Removes imputed sensors that lack complete data for 2018 and 2019, reducing the dataset from 48 to 20 sensors.
# - Calculates statistics for each sensor, including 
#   - total yearly ridership, 
#   - seasonal ridership, 
#   - weekend versus weekday ridership
# - Groups sensors into categories: 
#   - parallel to St-Denis REV, 
#   - leisure
#   - control (best for analysis of covid effects)
#   - untreated (no imputation or sensor removal for data issues)
# - Computes year-over-year percentage changes in ridership due to COVID-19 for each group
# - Exports the results as excel tables

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse", 
                       "openxlsx") 

for(Package in required_packages){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}

load(file = "SENSORS_imp_df(day).rda")
load(file = "SENSORS.rda")

#################################################################
# Summarize untreated data by day ------------------------------------------
#################################################################

Sensor_Split <- split(SENSORS, SENSORS$Name)

summarize_day <- function(df) {
  df %>% 
    group_by(Date) %>%
    summarize(Pass = sum(Pass, na.rm = TRUE)) %>%
    complete(Date = seq(min(Date), max(Date), by = "day")) %>% 
    arrange(Date) %>%
    as.data.frame()
}

Sensor_List_Untreated <- map(Sensor_Split, summarize_day)

#################################################################
# Prepare Imputed data ------------------------------------------
#################################################################

# Remove any sensors that do not have full data for 2018 and 2019
# This removes 28 sensors, so the count changes from N=48 to N=20
remove_list <- c(
  "16eAvenue_Belanger",
  "A25_Gouin",
  "Bennett_Ontario",
  "CamillienHoude1",
  "Estacade",
  "Maisonneuve_Greene",
  "Maisonneuve_Vendome",
  "MauriceDuplessis",
  "NotreDameEst_Bellerive",
  "ParcStanley",
  "PisteDesCarrieres",
  "PontIleBizard",
  "PontLeGardeur",
  "Rachel3_Angus",
  "Rachel_PieIX",
  "REV_Bellechasse_13eme",
  "REV_Bellechasse_AvChristopheColomb",
  "REV_Berri_SauveSB",
  "REV_Lajeunesse_SauveNB",
  "REV_StDenis_CarrieresNB",
  "REV_StDenis_CarrieresSB",
  "REV_StDenis_CastelnauNB",
  "REV_StDenis_CastelnauSB",
  "REV_StDenis_DuluthNB",
  "REV_StDenis_RachelSB",
  "SainteCroix_DuCollegeSainteCroix",
  "Souligny_Saintemile",
  "Valois_laFontaine"
)

Sensor_List_Reduced <- Sensor_List_Final[!(names(Sensor_List_Final) %in% remove_list)]

#################################################################
# Convert imputed List to a dataframe and calculate statistics ------------------------------------------
#################################################################

# Create Name column 
create_name <- function(df_list) {
  for (name in names(df_list)) {
    df_list[[name]]$Name <- name
  }
  return(df_list)
}

Sensor_List_Reduced <- create_name(Sensor_List_Reduced)
Sensor_List_Untreated <- create_name(Sensor_List_Untreated)

# Convert to dataframe and calculate statistics
convert_and_calculate <- function(list) {
  df <- bind_rows(list) %>%
    as.data.frame() %>%
    mutate(
      Season = ifelse(
        (month(Date) >= 3 & month(Date) <= 11) &
          !(month(Date) == 3 & day(Date) < 13) &
          !(month(Date) == 11 & day(Date) > 7), "High", "Low"),
      Week = ifelse(wday(Date) %in% c(1, 7), "Wend", "Wday"),
      Year = year(Date)
    ) %>%
    filter(Year >= 2019) %>%
    select(Date, Name, Pass, Year, Season, Week)
  
  stats <- df %>%
    group_by(Name, Year) %>%
    summarise(
      Wday = sum(Pass[Week == "Wday"]),
      Wend = sum(Pass[Week == "Wend"]),
      Low = sum(Pass[Season == "Low"]),
      High = sum(Pass[Season == "High"]),
      Pass = sum(Pass),
      .groups = 'drop'
    ) %>%
    mutate(
      Wend_Pct = round((Wend / (Wday + Wend)) * 100, 2),
      Low_Pct = round((Low / (Low + High)) * 100, 2),
      Change = if_else(Year == min(Year), 0, round(((Pass - lag(Pass)) / lag(Pass)) * 100, 2))
    ) %>%
    select(Year, Name, Wend_Pct, Low_Pct, Change, Pass, Wday, Wend, High, Low) %>%
    arrange(Year, Name)
  
  return(stats)
}

Sensor_Imputed <- convert_and_calculate(Sensor_List_Reduced)
Sensor_Untreated <- convert_and_calculate(Sensor_List_Untreated)


#################################################################
# Create sensor groups ------------------------------------------
#################################################################

# Parallel to St-Denis REV (6)
ll <- c("ChristopheColomb_Louvain", "Boyer_Everett", 
        "Boyer_Rosemont", "SaintLaurent_Bellechasse", 
        "Brebeuf_Rachel", "SaintUrbain")

# Control sensors (11)
ctrl <- c("CoteSainteCatherine_Stuart", 
          "Maisonneuve_Marcil", "Maisonneuve_Peel", 
          "NotreDame", "University_Milton", "Berri1", 
          "Rachel_HoteldeVille", "Rachel_Papineau", 
          "ReneLevesque_Wolfe", "Viger_SaintUrbain", 
          "Parc_Duluth")

# Leisure sensors (3)
leisure <- c("PierreDupuy","PontJacquesCartier","EdmondValade")

# Apply group names
Sensor_Imputed <- Sensor_Imputed %>%
  mutate(Group = case_when(
    Name %in% ll ~ "ll",
    Name %in% ctrl ~ "ctrl",
    Name %in% leisure ~ "leisure"
  ))

Sensor_Untreated <- Sensor_Untreated %>%
  mutate(Group = "Untreated")

# Summarize statistics for all sensors for each group
summarize_groups <- function(data, group_name, filter_condition) {
  data %>%
    filter(!!filter_condition) %>%
    group_by(Year) %>%
    summarize(
      Name = NA,
      Group = group_name,
      Pass = sum(Pass, na.rm = TRUE),
      Wday = sum(Wday, na.rm = TRUE),
      Wend = sum(Wend, na.rm = TRUE),
      High = sum(High, na.rm = TRUE),
      Low = sum(Low, na.rm = TRUE)
    ) %>%
    ungroup()
}

Total_all <- summarize_groups(Sensor_Imputed, "Control, leisure and parallel", expr(Group %in% c("ctrl", "leisure", "ll")))
Total_ctrl_leisure <- summarize_groups(Sensor_Imputed, "Control, leisure", expr(Group %in% c("ctrl", "leisure")))
Total_ctrl <- summarize_groups(Sensor_Imputed, "Control", expr(Group == "ctrl"))
Total_ll <- summarize_groups(Sensor_Imputed, "Parallel", expr(Group == "ll"))
Total_untreated <- summarize_groups(Sensor_Untreated, "Untreated", expr(Group == "Untreated"))

##############################################################
# Important correction : some sensors have partial or absent data for 2023
##############################################################

# Remove these sensors for 2019 and 2023 periods
Sensor_Imputed_2023 <- Sensor_Imputed %>%
  filter(Year %in% c(2019, 2023))%>%
  filter(!(Name %in% c("Maisonneuve_Peel", 
                       "PontJacquesCartier", 
                       "EdmondValade")))

Total_all_2023 <- summarize_groups(Sensor_Imputed_2023, "Control, leisure and parallel", expr(Group %in% c("ctrl", "leisure", "ll")))
Total_ctrl_leisure_2023 <- summarize_groups(Sensor_Imputed_2023, "Control, leisure", expr(Group %in% c("ctrl", "leisure")))
Total_ctrl_2023 <- summarize_groups(Sensor_Imputed_2023, "Control", expr(Group == "ctrl"))
Total_ll_2023 <- summarize_groups(Sensor_Imputed_2023, "Parallel", expr(Group == "ll"))

#################################################################
# Create table with Covid Change statistics ------------------------------------------
#################################################################

# Calculate change of each year compared to 2019
covid_change <- function(x) {
  df <- x %>%
    reframe(
      Year = Year,
      Group = Group,
      Pass = round(((Pass - Pass[Year == 2019]) / abs(Pass[Year == 2019])) * 100, 2),
      Wday = round(((Wday - Wday[Year == 2019]) / abs(Wday[Year == 2019])) * 100, 2),
      Wend = round(((Wend - Wend[Year == 2019]) / abs(Wend[Year == 2019])) * 100, 2),
      High = round(((High - High[Year == 2019]) / abs(High[Year == 2019])) * 100, 2),
      Low  = round(((Low - Low[Year == 2019]) / abs(Low[Year == 2019])) * 100, 2)
    ) %>% 
    filter(Year != "2019")%>% 
    select(Group, everything())
  return(df)
}

# Apply the covid change calculations function to each group
Stats_Untreated <- covid_change(Total_untreated)
Stats_all <- covid_change(Total_all)
Stats_ctrl_leisure <- covid_change(Total_ctrl_leisure)
Stats_ctrl <- covid_change(Total_ctrl)
Stats_ll <- covid_change(Total_ll)

Stats_all_2023 <- covid_change(Total_all_2023)
Stats_ctrl_leisure_2023 <- covid_change(Total_ctrl_leisure_2023)
Stats_ctrl_2023 <- covid_change(Total_ctrl_2023)
Stats_ll_2023 <- covid_change(Total_ll_2023)

# Overwrite the 2023 data with the specific calculations for this year
overwrite_2023 <- function(data, overwrite) {
  data <- data %>%
    mutate(across(everything(), ~ ifelse(Year == 2023, overwrite[[cur_column()]], .)))
  return(data)
}

Stats_all <- overwrite_2023(Stats_all,Stats_all_2023)
Stats_ctrl_leisure <- overwrite_2023(Stats_ctrl_leisure, Stats_ctrl_leisure_2023)
Stats_ctrl <- overwrite_2023(Stats_ctrl, Stats_ctrl_2023)
Stats_ll <- overwrite_2023(Stats_ll, Stats_ll_2023)

#################################################################
# Create table to compare covid drops by group ------------------------------------------
#################################################################

# Pivot tables and keep only covid drop statistics from total ridership
pivot <- function(x){
  df  <- x  %>%
    select(Year, Pass, Group) %>%
    pivot_wider(names_from = Year, values_from = Pass)
  return(df)
}

pivot_untreated <- pivot(Stats_Untreated)
pivot_all <- pivot(Stats_all)
pivot_ctrl_leisure <- pivot(Stats_ctrl_leisure)
pivot_ctrl <- pivot(Stats_ctrl)
pivot_ll <- pivot(Stats_ll)

# Combine tables
Stats_CompareGroups <- bind_rows(pivot_untreated,pivot_all,pivot_ctrl_leisure,pivot_ctrl,pivot_ll)

#################################################################
# Create Covid Change table for individual sensors------------------------------------------
#################################################################

# Pivot table and calculate covid change stats
pivot_covid_change <- function(x){
  df  <- x  %>%
    select(Year, Name, Wday, Wend, High, Low, Pass, Group) %>%
    pivot_wider(names_from = Year, values_from = c(Wday, Wend, High, Low, Pass))%>%
    mutate(
      Drop2020 = round(((Pass_2020-Pass_2019)/abs(Pass_2019))*100,2),
      Drop2021 = round(((Pass_2021-Pass_2019)/abs(Pass_2019))*100,2),
      Drop2022 = round(((Pass_2022-Pass_2019)/abs(Pass_2019))*100,2),
      Drop2023 = round(((Pass_2023-Pass_2019)/abs(Pass_2019))*100,2),
      Wday2020 = round(((Wday_2020-Wday_2019)/abs(Wday_2019))*100,2),
      Wday2021 = round(((Wday_2021-Wday_2019)/abs(Wday_2019))*100,2),
      Wday2022 = round(((Wday_2022-Wday_2019)/abs(Wday_2019))*100,2),
      Wday2023 = round(((Wday_2023-Wday_2019)/abs(Wday_2019))*100,2),
      Wend2020 = round(((Wend_2020-Wend_2019)/abs(Wend_2019))*100,2),
      Wend2021 = round(((Wend_2021-Wend_2019)/abs(Wend_2019))*100,2),
      Wend2022 = round(((Wend_2022-Wend_2019)/abs(Wend_2019))*100,2),
      Wend2023 = round(((Wend_2023-Wend_2019)/abs(Wend_2019))*100,2),
      High2020 = round(((High_2020-High_2019)/abs(High_2019))*100,2),
      High2021 = round(((High_2021-High_2019)/abs(High_2019))*100,2),
      High2022 = round(((High_2022-High_2019)/abs(High_2019))*100,2),
      High2023 = round(((High_2023-High_2019)/abs(High_2019))*100,2),
      Low2020  = round(((Low_2020-Low_2019)/abs(Low_2019))*100,2),
      Low2021  = round(((Low_2021-Low_2019)/abs(Low_2019))*100,2),
      Low2022  = round(((Low_2022-Low_2019)/abs(Low_2019))*100,2),
      Low2023  = round(((Low_2023-Low_2019)/abs(Low_2019))*100,2))%>%
    select(Group, Name, Drop2020, Drop2021, Drop2022, Drop2023, Wday2020, Wday2021, Wday2022, Wday2023,Wend2020, Wend2021, Wend2022, Wend2023, High2020, High2021, High2022, High2023, Low2020, Low2021, Low2022, Low2023) %>% 
    arrange(Group, Name)
  return(df)
}

# apply function
Stats_Imputed <- pivot_covid_change(Sensor_Imputed)


#################################################################
# Export results as excel tables ------------------------------------------
#################################################################

write.xlsx(Stats_Imputed, file = "Outputs/Tables/Stats_Individual.xlsx")
write.xlsx(Stats_CompareGroups, file = "Outputs/Tables/Stats_Grouped.xlsx")

