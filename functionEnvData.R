# processing environmental data

processing_env_data <- function(env_file = "./envVariables_with_stranding.csv", 
                                corresponding_df = pontoporia, 
                                id_column="id_individual"){
  envVariables <- as.data.frame(
    utils::read.csv(env_file))
  
  ## Remove id column
  envVariables <- envVariables %>% dplyr::select(-1)

  ## Melt env variables
  envVariables_melt <- reshape2::melt(envVariables, id.vars = "fileName")
  
  ## Extract only the info of interest (numeric part)
  envVariables_melt <- 
    envVariables_melt %>% 
    dplyr::mutate(value = gsub("dtype=float32", "", value)) %>% 
    dplyr::mutate(value = gsub("<xarray.Variable", "", value)) %>% 
    dplyr::mutate(value = gsub("[^0-9.e-]", "", value)) %>% 
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::mutate(fileName = gsub("WTUVP", "", fileName)) %>% 
    dplyr::mutate(fileName = gsub(".nc", "", fileName))

  
  ## Return df to wide format
  envVariablesNew <- tidyr::pivot_wider(envVariables_melt, 
                                        names_from = variable, 
                                        values_from = value)
  print(dim.data.frame(envVariablesNew))
  
  # calculate direction:
  envVariablesNew <- envVariablesNew %>%
    mutate(wind_speed_mean = sqrt(abs(u10mean)**2 + abs(v10mean)**2)) %>%
    mutate(wind_dir_mean = atan2(u10mean/sqrt((u10mean)^2 + (v10mean)^2), v10mean/sqrt(u10mean^2 + v10mean^2)) * 180/pi) %>%
    mutate(wind_dir_mean = ifelse(wind_dir_mean < 1, 180 + (180 - abs(wind_dir_mean)), wind_dir_mean)) %>%
    mutate(wind_speed_max = sqrt(abs(u10max)**2 + abs(v10max)**2))%>%
    mutate(wind_dir_max = atan2(u10max/sqrt((u10max)^2 + (v10max)^2), v10max/sqrt(u10max^2 + v10max^2)) * 180/pi) %>%
    mutate(wind_dir_max = ifelse(wind_dir_max < 1, 180 + (180 - abs(wind_dir_max)), wind_dir_max)) %>%
    mutate(wind_speed_min = sqrt(abs(u10min)**2 + abs(v10min)**2)) %>%
    mutate(wind_dir_min = atan2(u10min/sqrt((u10min)^2 + (v10min)^2), v10min/sqrt(u10min^2 + v10min^2)) * 180/pi) %>%
    mutate(wind_dir_min = ifelse(wind_dir_min < 1, 180 + (180 - abs(wind_dir_min)), wind_dir_min))
  
  ## Set equal ID's prior to merge
  corresponding_df[[id_column]] <- as.character(as.numeric(corresponding_df[[id_column]]))
  envVariablesNew$fileName <- as.character(as.numeric(envVariablesNew$fileName))
  
  ## Renaming Env columns
  strandingAndEnv <- 
    envVariablesNew %>% 
    dplyr::rename(u_wind_mean = u10mean,
                  u_wind_min = u10min,
                  u_wind_max = u10max,
                  v_wind_mean = v10mean,
                  v_wind_min = v10min,
                  v_wind_max = v10max,
                  mean_wave_dir_mean = mwdmean,
                  mean_wave_dir_max = mwdmax,
                  mean_wave_dir_min = mwdmin,
                  mean_wave_period_mean = mwpmean,
                  mean_wave_period_min = mwpmin,
                  mean_wave_period_max = mwpmax,
                  sig_height_wind_wave_mean = shwwmean,
                  sig_height_wind_wave_min = shwwmin,
                  sig_height_wind_wave_max = shwwmax)
  
  # Summarize "effort" and join 'strandingAndEnv' ##
  strandingAndEnv <- strandingAndEnv %>% dplyr::select(contains(c("fileName","mean")))
  ## Merge
  strandingAndEnv <- base::merge(corresponding_df, strandingAndEnv, 
                                 by.x = as.character(id_column), by.y = "fileName")%>% 
                                  dplyr::select(!contains(c("fileName", "_min", "_max")))
  
  strandingAndEnv
  
}

