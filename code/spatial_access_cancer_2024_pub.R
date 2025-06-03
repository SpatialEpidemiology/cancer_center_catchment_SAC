
###############################
##### RUN THIS FILE 1ST #######
###############################

library(sf)
library(tidyverse)
library(tigris)
library(readxl)
library(sociome)

sf_use_s2(FALSE)

#catchments
ct <- st_read("/YOUR_PATH/NCI_Catchment_Areas_fall2024-1/NCI_Catchment_Areas_fall2024.shp")
ct <- ct[which(!st_is_empty(ct)),]
ct <- st_transform(ct,crs=3857)
ct[64,1] <- "Mayo Clinic Comprehensive Cancer Center - Phoenix"
ct[65,1] <- "Mayo Clinic Comprehensive Cancer Center - Jacksonville"
ct[66,1] <- "Mayo Clinic Comprehensive Cancer Center - Rochester"


#crosswalk for CT tracts
cw <- read.csv("/YOUR_PATH/2022tractcrosswalk.csv")
cw <- cw[,c(1,2)]
cw$tract_fips_2020 <- str_pad(cw$tract_fips_2020,width=11,pad="0")
cw$Tract_fips_2022 <- str_pad(cw$Tract_fips_2022,width=11,pad="0")
cw <- arrange(cw,by_group=tract_fips_2020)

#geographic regions/divisions
div<-tigris::divisions()
div<-st_transform(div,crs=3857)

#tracts spatial
tr <- tracts(year=2023, cb=T)
tr <- tr[,5]
colnames(tr)[1] <- "id"

#urban areas 2020 # UA 2010 already attached
U20 <- read_delim("/YOUR_PATH/2020_UA_BLOCKS.txt")
colnames(U20)[9]<-"UA"
colnames(U20)[10]<-"UA_name"
U20 <- U20 %>% dplyr::select(STATE, COUNTY, TRACT, UA, UA_name) %>%
  mutate(id = paste(STATE, COUNTY, TRACT,sep="")) %>%
  distinct(id, .keep_all=TRUE) %>%
  dplyr::select(UA, UA_name, id)
vec0 <- U20$id[which(U20$id %in% cw$tract_fips_2020)] 
U20$id[which(U20$id %in% cw$tract_fips_2020)] <- cw$Tract_fips_2022[which(cw$tract_fips_2020 %in% vec0)]
U20$id[which(U20$id=="09009990000")] <- "09170990000"

#adi
adi <- read_excel("/YOUR_PATH/adi_tract_2023.xlsx")
colnames(adi)[1]<-"id"

#oncologists 2024
c1_c24 <- read_excel("/YOUR_PATH/oncologists_tr_catchments_access_1224.xlsx.xlsx")
c1_c24 <- left_join(c1_c24,tr,by="id")
c1_c24 <- st_as_sf(c1_c24)
c1_c24 <- st_transform(c1_c24,crs=3857)
c1_c24div <- st_intersection(c1_c24,div)
c1_c24div <- c1_c24div %>% st_drop_geometry() %>%
  distinct(., id, .keep_all = TRUE)
c1_c24div <- c1_c24div[,c(1:11,15)]
colnames(c1_c24div)[12]<-"div"
c1_c24div <- c1_c24div %>% left_join(., c1_c24[,c(1)], by="id") %>%
  st_as_sf()
c1_c24div <- left_join(c1_c24div,adi, by="id")
c1_c24ct <- st_intersection(c1_c24div, ct)
c1_c24ct$fid <- 1:nrow(c1_c24ct)
c1_c24ct2 <- c1_c24ct %>% merge(.,U20, by="id", all=T) %>%
  distinct(., fid, .keep_all = T)
anti_join(st_drop_geometry(c1_c24ct2),st_drop_geometry(c1_c24ct),by="id")
c1_c24ct2<-c1_c24ct2[-145,]
c1_c24ct2$count.x[which(c1_c24ct2$id=="39035198400")]<-NA
c1_c24ct2$UA[which(is.na(c1_c24ct2$UA))] <- "99999"
c1_c24ct<-c1_c24ct2

#cancer care providers
c2_c24 <- read_excel("/YOUR_PATH/cancercare_tr_catchments_access_1224.xlsx.xlsx")
c2_c24 <- left_join(c2_c24,tr,by="id")
c2_c24 <- st_as_sf(c2_c24)
c2_c24 <- st_transform(c2_c24,crs=3857)
c2_c24div <- st_intersection(c2_c24,div)
c2_c24div <- c2_c24div %>% st_drop_geometry() %>%
  distinct(., id, .keep_all = TRUE)
c2_c24div <- c2_c24div[,c(1:11,15)]
colnames(c2_c24div)[12]<-"div"
c2_c24div <- c2_c24div %>% left_join(., c2_c24[,c(1)], by="id") %>%
  st_as_sf()
c2_c24div <- left_join(c2_c24div,adi, by="id")
c2_c24ct <- st_intersection(c2_c24div, ct)
c2_c24ct$fid <- 1:nrow(c2_c24ct)
c2_c24ct2 <- c2_c24ct %>% merge(.,U20, by="id", all=T) %>%
  distinct(., fid, .keep_all = T)
anti_join(st_drop_geometry(c2_c24ct2),st_drop_geometry(c2_c24ct),by="id")
c2_c24ct2<-c2_c24ct2[-145,]
c2_c24ct2$count.x[which(c2_c24ct2$id=="39035198400")]<-NA
c2_c24ct2$UA[which(is.na(c2_c24ct2$UA))] <- "99999"
c2_c24ct<-c2_c24ct2

#primary care 2024
p24 <- read_excel("/YOUR_PATH/primarycare_tr_catchments_access_1224.xlsx.xlsx")
p24 <- left_join(p24,tr,by="id")
p24 <- st_as_sf(p24)
p24 <- st_transform(p24,crs=3857)
p24div <- st_intersection(p24,div)
p24div <- p24div %>% st_drop_geometry() %>%
  distinct(., id, .keep_all = TRUE)
p24div <- p24div[,c(1:11,15)]
colnames(p24div)[12]<-"div"
p24div <- p24div %>% left_join(., p24[,c(1)], by="id") %>%
  st_as_sf()
p24div <- left_join(p24div,adi, by="id")
p24ct <- st_intersection(p24div, ct)
p24ct$fid <- 1:nrow(p24ct)
p24ct2 <- p24ct %>% merge(.,U20, by="id", all=T) %>%
  distinct(., fid, .keep_all = T)
anti_join(st_drop_geometry(p24ct2),st_drop_geometry(p24ct),by="id")
p24ct2<-p24ct2[-145,]
p24ct2$count.x[which(p24ct2$id=="39035198400")]<-NA
p24ct2$UA[which(is.na(p24ct2$UA))] <- "99999"
p24ct<-p24ct2

#libraries for plotting
library(dplyr)
library(flextable)
library(purrr)
library(furrr)
library(tidyr)
library(progressr)

#set up parallel processing
handlers(global = FALSE)
plan(multisession, workers = availableCores() - 1)

# Define dataframes
df_names <- c("c1_c24ct", "c2_c24ct", "p24ct")

#processing functions
create_tables <- function(df) {
  if (inherits(df, "sf")) {
    df <- st_drop_geometry(df)
  }
  if ("geometry" %in% colnames(df)) {
    df <- df %>% dplyr::select(-geometry)
  }
  
  df <- df %>%
    mutate(
      `Urban/Rural Status` = ifelse(UA == "99999", "Rural", "Urban")
    ) 
  
  list(
    div_center = df %>%
      mutate(`Cancer Center` = name) %>%
      group_by(`Cancer Center`) %>%
      summarise(
        n = n(),
        Median = median(round(count.x*100000, digits = 3), na.rm = TRUE),
        Min = min(round(count.x*100000, digits = 3), na.rm = TRUE),
        Max = max(round(count.x*100000, digits = 3), na.rm = TRUE),
        .groups = 'drop'
      ) %>% 
      arrange(desc(Median)),
    
    division = df %>%
      mutate(`Census Division` = div) %>%
      group_by(`Census Division`) %>%
      distinct(., id, .keep_all = T) %>%
      summarise(
        n = n(),
        Median = median(round(count.x*100000, digits = 3), na.rm = TRUE),
        Min = min(round(count.x*100000, digits = 3), na.rm = TRUE),
        Max = max(round(count.x*100000, digits = 3), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Median)),
    
    adi_urban = df %>%
      mutate(
        `ADI Quartile` = ntile(ADI, 4)) %>%
      group_by(`ADI Quartile`, `Urban/Rural Status`) %>%
      filter(!is.na(`ADI Quartile`)) %>%
      distinct(., id, .keep_all = T) %>%
      summarise(
        n = n(),
        Median = median(round(count.x*100000, digits = 3), na.rm = TRUE),
        Min = min(round(count.x*100000, digits = 3), na.rm = TRUE),
        Max = max(round(count.x*100000, digits=3), na.rm = TRUE),
        .groups = 'drop'
      ),
    adi = df %>%
      mutate(
        `ADI Quartile` = ntile(ADI, 4)) %>%
      group_by(`ADI Quartile`) %>%
      filter(!is.na(`ADI Quartile`)) %>%
      distinct(., id, .keep_all = T) %>%
      summarise(
        n = n(),
        Median = median(round(count.x*100000, digits = 3), na.rm = TRUE),
        Min = min(round(count.x*100000, digits = 3), na.rm = TRUE),
        Max = max(round(count.x*100000, digits=3), na.rm = TRUE),
        .groups = 'drop'
      ),
    urban = df %>%
      group_by(`Urban/Rural Status`) %>%
      filter(!is.na(`Urban/Rural Status`)) %>%
      distinct(., id, .keep_all = T) %>%
      summarise(
        n = n(),
        Median = median(round(count.x*100000, digits = 3), na.rm = TRUE),
        Min = min(round(count.x*100000, digits = 3), na.rm = TRUE),
        Max = max(round(count.x*100000, digits=3), na.rm = TRUE),
        .groups = 'drop'
      )
  )
}

format_table <- function(data) {
  set_flextable_defaults(background.color = "white")
  ft <- flextable(data) %>%
    theme_booktabs() %>%
    autofit() %>%
    align(align = "left", part = "all") %>%
    colformat_double(j = c("Median", "Min", "Max"), digits = 2)
  
  if ("div" %in% colnames(data) && "name" %in% colnames(data)) {
    ft <- merge_v(ft, j = "`Census Division`")
  } else if ("`ADI Quartile`" %in% colnames(data)) {
    ft <- merge_v(ft, j = "`ADI Quartile`")
  }
  
  return(ft)
}

#process data
results <- map(setNames(nm = df_names), ~ {
  message(paste("Processing", .x))
  df <- get(.x)
  
  tables <- create_tables(df)
  
  list(
    formatted = list(
      div_center = format_table(tables$div_center),
      division = format_table(tables$division),
      adi_urban = format_table(tables$adi_urban),
      adi = format_table(tables$adi), 
      urban = format_table(tables$urban)
    ),
    raw = tables
  )
})

plan(sequential)

#save individual cancer center figures
#oncology
save_as_image(results$c1_c24ct$formatted$div_center,"/YOUR_PATH/c1_c24_div_center.png")
#cancer care
save_as_image(results$c2_c24ct$formatted$div_center,"/YOUR_PATH/c2_c24_div_center.png")
#primary care
save_as_image(results$p24ct$formatted$div_center,"/YOUR_PATH/p24_div_center.png")


#adding p-values
library(rstatix)
library(flextable)

#add p-values from Wilcoxon tests to tables
add_pvalues <- function() {
  datasets <- list(
    c1_c24ct = c1_c24ct,
    c2_c24ct = c2_c24ct,
    p24ct = p24ct
  )
  
  results_w_pvalues <- list()
  
  for (dataset_name in names(datasets)) {
    df <- datasets[[dataset_name]]
    
    if (inherits(df, "sf")) {
      df <- st_drop_geometry(df)
    }
    
    df <- df %>%
      mutate(
        `Urban/Rural Status` = ifelse(UA == "99999", "Rural", "Urban"),
        `ADI Quartile` = ntile(ADI, 4),
        count_per_100k = round(count.x*100000, digits = 3)
      )
    
    #ADI quartile tests
    adi_data <- df %>% 
      filter(!is.na(`ADI Quartile`)) %>%
      distinct(id, .keep_all = TRUE)
    
    adi_table <- adi_data %>%
      group_by(`ADI Quartile`) %>%
      summarise(
        n = n(),
        Median = median(count_per_100k, na.rm = TRUE),
        Min = min(count_per_100k, na.rm = TRUE),
        Max = max(count_per_100k, na.rm = TRUE),
        .groups = 'drop'
      )
    
    #run Wilcoxon tests 
    adi_pvalues <- c(NA) 
    ref_data <- adi_data %>% filter(`ADI Quartile` == 1) %>% pull(count_per_100k)
    
    for (q in 2:4) {
      comparison_data <- adi_data %>% filter(`ADI Quartile` == q) %>% pull(count_per_100k)
      test_result <- wilcox.test(comparison_data, ref_data)
      adi_pvalues <- c(adi_pvalues, test_result$p.value)
    }
    
    adi_table$p_value <- adi_pvalues
    
    #ADI quartile by urban/rural
    adi_urban_table <- df %>%
      filter(!is.na(`ADI Quartile`)) %>%
      distinct(id, .keep_all = TRUE) %>%
      group_by(`ADI Quartile`, `Urban/Rural Status`) %>%
      summarise(
        n = n(),
        Median = median(count_per_100k, na.rm = TRUE),
        Min = min(count_per_100k, na.rm = TRUE),
        Max = max(count_per_100k, na.rm = TRUE),
        .groups = 'drop'
      )
    
    adi_urban_pvalues <- c()
    
    adi_urban_temp <- data.frame()
    
    for (q in 1:4) {
      q_data <- df %>% 
        filter(!is.na(`ADI Quartile`), `ADI Quartile` == q) %>%
        distinct(id, .keep_all = TRUE)
      
      urban_data <- q_data %>% filter(`Urban/Rural Status` == "Urban") %>% pull(count_per_100k)
      rural_data <- q_data %>% filter(`Urban/Rural Status` == "Rural") %>% pull(count_per_100k)
      
      urban_stats <- q_data %>%
        filter(`Urban/Rural Status` == "Urban") %>%
        summarise(
          `ADI Quartile` = unique(q),
          `Urban/Rural Status` = "Urban",
          n = n(),
          Median = median(count_per_100k, na.rm = TRUE),
          Min = min(count_per_100k, na.rm = TRUE),
          Max = max(count_per_100k, na.rm = TRUE),
          p_value = NA  # Urban is reference, so NA
        )
      
      rural_stats <- q_data %>%
        filter(`Urban/Rural Status` == "Rural") %>%
        summarise(
          `ADI Quartile` = unique(q),
          `Urban/Rural Status` = "Rural",
          n = n(),
          Median = median(count_per_100k, na.rm = TRUE),
          Min = min(count_per_100k, na.rm = TRUE),
          Max = max(count_per_100k, na.rm = TRUE)
        )
      
      if (length(urban_data) > 0 && length(rural_data) > 0) {
        test_result <- wilcox.test(rural_data, urban_data)
        rural_stats$p_value <- test_result$p.value
      } else {
        rural_stats$p_value <- NA
      }
      
      adi_urban_temp <- bind_rows(adi_urban_temp, urban_stats, rural_stats)
    }
    
    adi_urban_table <- adi_urban_temp
    
    #Census divisions
    if (dataset_name %in% c("c1_c24ct", "c2_c24ct")) {
      ref_division <- "Middle Atlantic"
    } else {  # p24ct
      ref_division <- "West North Central"
    }
    
    division_table <- df %>%
      distinct(id, .keep_all = TRUE) %>%
      group_by(div) %>%
      summarise(
        n = n(),
        Median = median(count_per_100k, na.rm = TRUE),
        Min = min(count_per_100k, na.rm = TRUE),
        Max = max(count_per_100k, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Median))  # Add this line
    
    colnames(division_table)[1] <- "Census Division"
    
    #run Wilcoxon tests
    division_pvalues <- c()
    ref_data <- df %>% 
      filter(div == ref_division) %>% 
      distinct(id, .keep_all = TRUE) %>%
      pull(count_per_100k)
    
    for (division in unique(division_table$`Census Division`)) {
      if (division == ref_division) {
        division_pvalues <- c(division_pvalues, NA)  # Reference division
      } else {
        comparison_data <- df %>% 
          filter(div == division) %>% 
          distinct(id, .keep_all = TRUE) %>%
          pull(count_per_100k)
        
        test_result <- wilcox.test(comparison_data, ref_data)
        division_pvalues <- c(division_pvalues, test_result$p.value)
      }
    }
    
    division_table$p_value <- division_pvalues
    
    results_w_pvalues[[dataset_name]] <- list(
      adi = format_pvalue_table(adi_table),
      adi_urban = format_pvalue_table(adi_urban_table),
      division = format_pvalue_table(division_table)
    )
  }
  
  return(results_w_pvalues)
}

#format tables w/ p-values
format_pvalue_table <- function(data) {
  data <- data %>%
    mutate(
      p_formatted = case_when(
        is.na(p_value) ~ "Ref",
        p_value < 0.001 ~ "<0.001***",
        p_value < 0.01 ~ paste0(format(p_value, digits = 3), "**"),
        p_value < 0.05 ~ paste0(format(p_value, digits = 3), "*"),
        TRUE ~ format(p_value, digits = 3)
      )
    ) %>%
    dplyr::select(-p_value) %>%
    rename(`P-value` = p_formatted)
  
  # Create flextable
  ft <- flextable(data) %>%
    theme_booktabs() %>%
    autofit() %>%
    align(align = "left", part = "all") %>%
    colformat_double(j = c("Median", "Min", "Max"), digits = 2)
  
    ft <- add_footer_lines(ft, 
                         "* p<0.05, ** p<0.01, *** p<0.001. P-values from Wilcoxon rank sum tests."
  )
  
  if ("`ADI Quartile`" %in% colnames(data)) {
    ft <- merge_v(ft, j = "`ADI Quartile`")
  }
  
  return(ft)
}

result_tables <- add_pvalues()


#result viewing
result_tables$c1_c24ct$adi
result_tables$c1_c24ct$adi_urban
result_tables$c1_c24ct$division
result_tables$c2_c24$adi
result_tables$c2_c24$adi_urban
result_tables$p24ct$division
result_tables$p24$adi
result_tables$p24$adi_urban
result_tables$p24ct$division

#produce figures for adi, adi-urban, census division by provider type
#oncology
save_as_image(result_tables$c1_c24ct$adi, "/YOUR_PATH/c1_c24_adi.png")
save_as_image(result_tables$c1_c24ct$adi_urban, "/YOUR_PATH/c1_c24_adi_urban.png")
save_as_image(result_tables$c1_c24ct$division, "/YOUR_PATH/c1_c24_division.png")

#cancer care
save_as_image(result_tables$c2_c24ct$adi, "/YOUR_PATH/c2_c24_adi.png")
save_as_image(result_tables$c2_c24ct$adi_urban, "/YOUR_PATH/c2_c24_adi_urban.png")
save_as_image(result_tables$c2_c24ct$division, "/YOUR_PATH/c2_c24_division.png")

#primary care
save_as_image(result_tables$p24ct$adi, "/YOUR_PATH/p24_adi.png")
save_as_image(result_tables$p24ct$adi_urban, "/YOUR_PATH/p24_adi_urban.png")
save_as_image(result_tables$p24ct$division, "/YOUR_PATH/p24_division.png")


#p-trend in stratified rural-urban +adi
library(coin)
c1_c24ct <- c1_c24ct %>% mutate(ur_rec = ifelse(UA=="99999",2,1))
c1_c24ct$ur_rec <- factor(c1_c24ct$ur_rec)
c1_c24ct <- c1_c24ct %>% mutate(ADI_rec=ntile(ADI,4))
c1_c24ct$ADI_rec <- as.factor(c1_c24ct$ADI_rec)
c2_c24ct <- c2_c24ct %>% mutate(ur_rec = ifelse(UA=="99999",2,1))
c2_c24ct$ur_rec <- factor(c2_c24ct$ur_rec)
c2_c24ct <- c2_c24ct %>% mutate(ADI_rec=ntile(ADI,4))
c2_c24ct$ADI_rec <- as.factor(c2_c24ct$ADI_rec)
p24ct <- p24ct %>% mutate(ur_rec = ifelse(UA=="99999",2,1))
p24ct$ur_rec <- factor(p24ct$ur_rec)
p24ct <- p24ct %>% mutate(ADI_rec=ntile(ADI,4))
p24ct$ADI_rec <- as.factor(p24ct$ADI_rec)

#ordered factors
c1_c24ct$ADI_rec <- factor(c1_c24ct$ADI_rec, levels = 1:4, ordered = TRUE)
c1_c24ct$ur_rec <- factor(c1_c24ct$ur_rec, levels = c(1, 2), labels = c("Urban", "Rural"))
c2_c24ct$ADI_rec <- factor(c2_c24ct$ADI_rec, levels = 1:4, ordered = TRUE)
c2_c24ct$ur_rec <- factor(c2_c24ct$ur_rec, levels = c(1, 2), labels = c("Urban", "Rural"))
p24ct$ADI_rec <- factor(p24ct$ADI_rec, levels = 1:4, ordered = TRUE)
p24ct$ur_rec <- factor(p24ct$ur_rec, levels = c(1, 2), labels = c("Urban", "Rural"))

#oncology
#detect dominant trend direction
get_trend_direction <- function(data) {
  medians <- tapply(data$count.x, data$ADI_rec, median, na.rm = TRUE)
  
  if (all(diff(medians) >= 0)) {
    return("greater")
  } else if (all(diff(medians) <= 0)) {
    return("less")
  }
  
  if (tail(medians, 1) > head(medians, 1)) {
    return("greater")
  } else {
    return("less")
  }
}

#run test with correct direction
run_stratified_trend_test <- function(group_label) {
  data_sub <- subset(c1_c24ct, ur_rec == group_label)
  direction <- get_trend_direction(data_sub)
  
  test <- coin::independence_test(count.x ~ ADI_rec,
                                  data = data_sub,
                                  teststat = "quad",
                                  distribution = coin::approximate(B = 10000),
                                  alternative = direction)
  
  data.frame(
    group = group_label,
    trend_direction = direction,
    statistic = coin::statistic(test),
    p_value = coin::pvalue(test)
  )
}

#urban/rural
results <- bind_rows(
  run_stratified_trend_test("Urban"),
  run_stratified_trend_test("Rural")
)

print(results)

#cancer care
#run test with correct direction
run_stratified_trend_test2 <- function(group_label) {
  data_sub <- subset(c2_c24ct, ur_rec == group_label)
  direction <- get_trend_direction(data_sub)
  
  test <- coin::independence_test(count.x ~ ADI_rec,
                                  data = data_sub,
                                  teststat = "quad",
                                  distribution = coin::approximate(B = 10000),
                                  alternative = direction)
  
  data.frame(
    group = group_label,
    trend_direction = direction,
    statistic = coin::statistic(test),
    p_value = coin::pvalue(test)
  )
}

results2 <- bind_rows(
  run_stratified_trend_test2("Urban"),
  run_stratified_trend_test2("Rural")
)

print(results2)

#primary care
#run test with correct direction
run_stratified_trend_test3 <- function(group_label) {
  data_sub <- subset(p24ct, ur_rec == group_label)
  direction <- get_trend_direction(data_sub)
  
  test <- coin::independence_test(count.x ~ ADI_rec,
                                  data = data_sub,
                                  teststat = "quad",
                                  distribution = coin::approximate(B = 10000),
                                  alternative = direction)
  
  data.frame(
    group = group_label,
    trend_direction = direction,
    statistic = coin::statistic(test),
    p_value = as.numeric(coin::pvalue(test))
  )
}

results3 <- bind_rows(
  run_stratified_trend_test3("Urban"),
  run_stratified_trend_test3("Rural")
)

print(results3)


library(flextable)
library(officer)
library(dplyr)

provider_names <- c("Oncology", "Cancer care", "Primary care")

provider_classification <- rep(provider_names, each = 2)

# Create data frame
table_data <- data.frame(
  `Provider Classification` = provider_classification,
  `Urban/Rural Status` = rep(c("Urban", "Rural"), 3),
  `ADI Quartile Trend Direction` = c("Increasing", "Decreasing", "Increasing", "Decreasing", "Increasing", "Increasing"),
  `Test Statistic` = c(436.525, 52.64003, 413.85776, 18.57664, 1506.04849, 11.80905),
  `P-value` = c(0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.0004),  # Numeric for easier handling
  check.names = FALSE
)

table_data$`Test Statistic` <- sprintf("%.2f", table_data$`Test Statistic`)

# Function to add significance stars
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

table_data$`P-value Numeric` <- table_data$`P-value`

table_data <- table_data %>%
  mutate(
    `P-value` = paste0(
      ifelse(`P-value Numeric` < 0.0001, "<0.0001", sprintf("%.4f", `P-value Numeric`)),
      "", 
      sapply(`P-value Numeric`, add_stars)
    )
  ) %>%
  select(-`P-value Numeric`)

#flextable
ft <- flextable(table_data)

#theme
ft <- theme_booktabs(ft)

#footer 
ft <- add_footer_lines(ft, values = "* p<0.05, ** p<0.01, *** p<0.001 (Jonckheere–Terpstra trend test)")

#print
ft


#medians and wicoxon rank sum for comprehensive vs non-comp cancer center
c1_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
c1_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
c1 <- c1_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) 
c1_2 <- c1_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) 
wilcox.test(c1,c1_2)

c2_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
c2_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
c2 <- c2_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) 
c2_2 <- c2_c24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) 
wilcox.test(c2,c2_2)

p24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
p24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) %>%
  median(na.rm=T)
p <- p24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Comprehensive Cancer Center") %>%
  pull(count.x) 
p_2 <- p24ct %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(type=="Cancer Center") %>%
  pull(count.x) 
wilcox.test(p,p_2)


#classifications
provider_names <- c("Oncology", "Cancer care", "Primary care")

#median values
median_comp <- c(0.0001152407, 0.00103751, 0.0005523362)
median_cc <- c(0.0001085429, 0.001079942, 0.0004891085)
p_vals <- c(1.678e-13, 0.0002248, 2.2e-16)

#access per 100k providers
median_comp <- median_comp * 100000
median_cc <- median_cc * 100000

table_data <- data.frame(
  `Provider Classification` = provider_names,
  `Median (Comprehensive Cancer Centers)` = median_comp,
  `Median (Cancer Centers)` = median_cc,
  `P-value` = p_vals,
  check.names = FALSE
)

table_data$`Median (Comprehensive Cancer Centers)` <- sprintf("%.2f", table_data$`Median (Comprehensive Cancer Centers)`)
table_data$`Median (Cancer Centers)` <- sprintf("%.2f", table_data$`Median (Cancer Centers)`)

#add significance
add_stars <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

#p-values and add stars
table_data <- table_data %>%
  mutate(
    `P-value Numeric` = p_vals,
    `P-value` = paste0(
      ifelse(is.na(`P-value Numeric`), "NA",
             ifelse(`P-value Numeric` < 0.0001, "<0.0001", sprintf("%.4f", `P-value Numeric`))),
      sapply(`P-value Numeric`, add_stars)
    )
  ) %>%
  select(-`P-value Numeric`)

access_comparison_table <- flextable(table_data)

#styling
access_comparison_table <- theme_booktabs(access_comparison_table)
access_comparison_table <- autofit(access_comparison_table)
access_comparison_table <- add_footer_lines(access_comparison_table, values = "* p<0.05, ** p<0.01, *** p<0.001. P-values from Wilcoxon rank sum tests.")

#print
access_comparison_table






