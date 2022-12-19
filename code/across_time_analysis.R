##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Timeseries analysis (KPSS, changepoints & bootstrap)
## Author: Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Dynamics of collective motion across time and species
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


###########################
## Bootstrap functions
to_boot_sp <- function(df, var, indices)
{
  df <- df[indices,]
  
  df$var_mean <- dplyr::cummean(df[,var])
  df$cum_dur <- cumsum(df$event_dur_s)
  df$event_ord <- 1:nrow(df)
  sc <- lm.br::lm.br( df$var_mean ~ df$event_ord, 'LT' )
  theta_d <- df[df$event_ord == round(as.numeric(sc$coef[1])), 'cum_dur']
  
  return(theta_d)
}

bootstrap_all <- function(df, vars, R)
{
  ret_vars <- c('var', 'theta_idx', 'sl','ci_l', 'ci_h', 'theta_s', 'ci_ls', 'ci_hs', 'boots_sd')
  toret <- as.data.frame(matrix(NA, nrow = length(vars), ncol = length(ret_vars)))
  colnames(toret) <- ret_vars
  
  k <- 1
  for (i in vars)
  {
    # get main plateu calculations
    plat_stats <- get_plateau_stats(df, i)
    
    # run bootstrap
    res <- boot::boot(data = df,
                      statistic = to_boot_sp,
                      R = R, 
                      var = i)
    
    res <- as.vector(res$t)
    toret[k,] <- c(plat_stats, sd(res))
    k <- k + 1
  }
  return(toret)
}


get_plateau_stats <- function(df, var)
{
  df$var_mean <- dplyr::cummean(df[,var])
  df$cum_dur <- cumsum(df$event_dur_s)
  df$event_ord <- 1:nrow(df)
  sc <- lm.br::lm.br( df$var_mean ~ df$event_ord, 'LT' )

  theta <- as.numeric(sc$coef[1])
  sl_capt <- capture.output(sc$sl(theta0 = max(df$event_ord)))
  signl <- gsub(" ", "",sub("\\ for .*", "", sub(".*\\SL=", "", sl_capt)), fixed = TRUE)
    
  p_ci <- capture.output(sc$ci())
  p_ci <- p_ci[length(p_ci)]
  if (stringr::str_count( p_ci, "], ") > 0)
  {
    splstr <- unlist(stringr::str_split(p_ci, '], '))
    p_ci <- splstr[length(splstr)]
  }
  pci_l <- floor(as.numeric(gsub("[^0-9.-]", "", sub("\\,.*", "", p_ci))))
  pci_h <- ceiling(as.numeric(gsub("[^0-9.-]", "", sub(".*\\,", "", p_ci))))
  if (!(is.na(pci_l)) && is.na(pci_h)) { pci_h <- max(df$event_ord, na.rm = T)}
  if (is.na(pci_l)) { pci_h <- NA }
    
  theta_s <- df[df$event_ord == round(as.numeric(theta)),'cum_dur']
  if (!(is.na(pci_l))){
    cumdur_l <- df[df$event_ord == pci_l,'cum_dur']
  } else {
    cumdur_l <- NA
  }
  
  if (!(is.na(pci_h))){
    cumdur_h <- df[df$event_ord == pci_h,'cum_dur']
  } else {
    cumdur_h <- NA
  }

  toret <- c(var, theta, signl, pci_l, pci_h, theta_s, cumdur_l, cumdur_h)
  return(toret)
}

###########################
## Load data 
df_b <- read.csv('../data/col_motion_metrics/metrics_per_event_baboons.csv')
df_f <- read.csv('../data/col_motion_metrics/metrics_per_event_fish.csv')
df_g <- read.csv('../data/col_motion_metrics/metrics_per_event_goats.csv')
df_p <- read.csv('../data/col_motion_metrics/metrics_per_event_pigeons.csv')

vars <- c('mean_mean_nnd', 
          'mean_sd_nnd', 
          'sd_mean_nnd',
          'mean_pol', 
          'sd_pol',
          'stdv_speed',
          'mean_sd_front', 
          'mean_shape',
          'sd_shape')

###########################
## Run bootstrap over all parameters

bt_g <- bootstrap_all(df_g, vars, 10000)
bt_g$species <- 'goats'

bt_f <- bootstrap_all(df_f, vars, 10000)
bt_f$species <- 'fish'

bt_b <- bootstrap_all(df_b, vars, 10000)
bt_b$species <- 'baboons'

bt_p <- bootstrap_all(df_p, vars, 10000)
bt_p$species <- 'pigeons'

all_boots <- dplyr::bind_rows(bt_f, bt_p, bt_g, bt_b)

all_boots$sl_bool <- FALSE
signifs <- as.numeric(all_boots$sl) < 0.05
all_boots$sl_bool <- signifs

###########################
## KPSS test for stationarity

kpss_tests <- as.data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(kpss_tests) <- c('fish', 'pigeons', 'goats', 'baboons')
for (var in vars)
{
  kpss_tests[var, ] <- c(round(tseries::kpss.test(dplyr::cummean(df_f[,var]), null = "Trend")$p.value, 3),
                      round(tseries::kpss.test(dplyr::cummean(df_p[,var]), null = "Trend")$p.value, 3),
                      round(tseries::kpss.test(dplyr::cummean(df_g[,var]), null = "Trend")$p.value, 3),
                      round(tseries::kpss.test(dplyr::cummean(df_b[,var]), null = "Trend")$p.value, 3))
}

kpss_tests$var <- rownames(kpss_tests)
kpss_tests <- reshape2::melt(kpss_tests)
kpss_tests$kpss_bool <- kpss_tests$value < 0.05
colnames(kpss_tests) <- c('var', 'species', 'kpss_pvalue', 'kpss_bool')

## Join and save
res_df <- dplyr::inner_join(all_boots, kpss_tests)
#write.csv(res_df, '../data/changepoints_stats.csv', row.names = FALSE)
