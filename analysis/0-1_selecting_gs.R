# selecting GS
rm(list=ls())
pacman::p_load(tidyverse, gtools, bigleaf)
FLUXNET2015_Folder <- ".../FLUXNET2015/" # a local folder storing FLUXNET2015 files
f <- list.files(path= FLUXNET2015_Folder, pattern= ".zip",full.names=FALSE, recursive=FALSE)
SiteInfo <- read.csv(".../file/SiteInfo/FLUXNET2015.csv")

df.main <- data.frame()

for (i in 1:length(f)){

  SiteName <- substr(f[i],5,10)
  SiteLat <- SiteInfo$Latitude[SiteInfo$Name == SiteName]
  SiteLon <- SiteInfo$Longitude[SiteInfo$Name == SiteName]
  SitePFT <- SiteInfo$PFT[SiteInfo$Name == SiteName]
  
  message(paste0("processing ",i, ", ",SiteName))
  
  # create temporary directory
  tempdir = ".../FLUXNET2015/tempdir/"
  if (dir.exists(tempdir)){
    unlink(tempdir, recursive = TRUE)
  }
  dir.create(tempdir)
  site_file_dir = paste0(FLUXNET2015_Folder,f[i])
  unzip(site_file_dir,exdir=tempdir)
  ff <- list.files(tempdir,pattern=paste0("FLX_", SiteName, "_FLUXNET2015_FULLSET_DD"))
  
  # GS is indicated by a GPP value > 0.5 GPPmax
  df <- read.csv(paste0(tempdir, ff)) %>% 
    select(TIMESTAMP, GPP_NT_VUT_REF) %>% 
    mutate(Site = SiteName, 
           year = as.numeric(substr(as.character(TIMESTAMP), 1, 4)),
           month = as.numeric(substr(as.character(TIMESTAMP), 5, 6)),
           day = as.numeric(substr(as.character(TIMESTAMP), 7, 8))) %>% 
    group_by(year) %>% 
    mutate(GS_flag = filter.growing.season(GPP_NT_VUT_REF, 0.5, ws = 15, min.int = 5)) %>% 
    select(-GPP_NT_VUT_REF)
    
  df.main = rbind(df.main, df)
}



