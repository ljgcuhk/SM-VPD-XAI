rm(list=ls())
pacman::p_load(tidyverse, gtools)
InputFolder <- ".../file/"
SiteInfo <- read.csv(paste0(InputFolder,"SiteInfo/SiteInfo_5yr.csv")) %>% 
  drop_na(VPD_SM_Site_SM3) %>% # this metric indicates if all variables (especially SM1-3) is available for forests
  filter(PFT != "CRO", PFT != "GRA", PFT != "WSH")

FLUXNET2015_Folder <- ".../FLUXNET2015/" # a local folder storing FLUXNET2015 files
GS_flag <- read.csv(paste0(InputFolder,"SiteInfo/GS_flag.csv")) # indicating growing seasons

df.main_H <- data.frame()

for (i in 1:nrow(SiteInfo)){

  SiteName <- SiteInfo$Site[i]
  SiteLat <- SiteInfo$Latitude[i]
  SiteLon <- SiteInfo$Longitude[i]
  SitePFT <- SiteInfo$PFT[i]
  
  message(paste0("processing ",i, ", ",SiteName))
  
  tempdir = "/Users/jiangongliu/Desktop/Columbia/FLUXNET2015/tempdir/"
  if (dir.exists(tempdir)){
    unlink(tempdir, recursive = TRUE)
  }
  
  dir.create(tempdir)
  
  site_file_name <- list.files(path= FLUXNET2015_Folder, pattern=paste0("FLX_",SiteName,"_FLUXNET2015_"), 
                               full.names=FALSE, recursive=FALSE)
  site_file_dir = paste0(FLUXNET2015_Folder,site_file_name)
  unzip(site_file_dir,exdir=tempdir)
  
  f_H <- list.files(tempdir, pattern=paste0("FLX_", SiteName, "_FLUXNET2015_FULLSET_H"))
  
  HourIndex <- substr(f_H, 32, 33)

  if (HourIndex == "HH"){
    nHour <- 48
  } else {nHour <- 24}
  
  GS_site <- GS_flag %>% filter(Site == SiteName) %>% select(year, GS_flag)

  ##extracting hourly data
  df_H <- read.csv(paste0(tempdir, f_H)) %>% 
    select(TIMESTAMP_START, TA_F, TA_F_QC, SW_IN_F, SW_IN_F_QC, VPD_F, VPD_F_QC,
           P_F, P_F_QC, WS_F, WS_F_QC, CO2_F_MDS, CO2_F_MDS_QC, 
           SWC_F_MDS_1, SWC_F_MDS_1_QC,SWC_F_MDS_2, SWC_F_MDS_2_QC,
           SWC_F_MDS_3, SWC_F_MDS_3_QC,
           GPP_NT_VUT_REF, GPP_DT_VUT_REF, NEE_VUT_REF, NEE_VUT_REF_QC) %>% 
    mutate(across(where(is.character), ~ na_if(.x, "-9999")),
          across(where(is.numeric), ~ na_if(.x, -9999))) %>% 
    mutate(Site = SiteName, Lat = SiteLat, Lon = SiteLon, PFT = SitePFT,
           year = as.numeric(substr(as.character(TIMESTAMP_START), 1, 4)),
           month = as.numeric(substr(as.character(TIMESTAMP_START), 5, 6)),
           TA = ifelse(TA_F_QC < 2, TA_F, NA),
           SW = ifelse(SW_IN_F_QC < 2, SW_IN_F, NA),
           VPD = ifelse(VPD_F_QC < 2, VPD_F, NA),
           Prec = ifelse(P_F_QC < 2, P_F, NA),
           WS = ifelse(WS_F_QC < 2, WS_F, NA),
           CO2 = ifelse(CO2_F_MDS_QC < 2, CO2_F_MDS, NA),
           SWC1 = ifelse(SWC_F_MDS_1_QC < 2, SWC_F_MDS_1, NA),
           SWC2 = ifelse(SWC_F_MDS_2_QC < 2, SWC_F_MDS_2, NA),
           SWC3 = ifelse(SWC_F_MDS_3_QC < 2, SWC_F_MDS_3, NA),
           SWCmean = (SWC1 + SWC2 + SWC3) / 3,
           GPP_NT = ifelse(NEE_VUT_REF_QC == 0, GPP_NT_VUT_REF, NA),
           GPP_DT = ifelse(NEE_VUT_REF_QC == 0, GPP_DT_VUT_REF, NA),
           NEE = ifelse(NEE_VUT_REF_QC == 0, NEE_VUT_REF, NA),
           GS_flag = rep(GS_site$GS_flag, each = nHour)) %>% 
    relocate(Site) %>% relocate(Lat, .after = Site) %>% 
    relocate(Lon, .after = Lat) %>% relocate(PFT, .after = Lon) %>% 
    filter(TA > -5, GS_flag == 1) %>% 
    select(Site, Lat, Lon, PFT, TA, SW, VPD, Prec, WS, CO2, SWC1, SWC2, SWC3,
           SWCmean, GPP_NT, GPP_DT, NEE) %>% drop_na()
 
   df.main_H = rbind(df.main_H, df_H)
}



