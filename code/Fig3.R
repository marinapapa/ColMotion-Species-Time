##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 3
## Author: Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Dynamics of collective motion across time and species
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## For exact replication of the figures the font Palatino Linotype is needed
#extrafont::loadfonts('win')

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# A. Heatmap ##########################################

toheat <- read.csv('../data/changepoints_stats.csv')

toheat$spec_tag <- 4
toheat[toheat$species == 'pigeons','spec_tag'] <- 3
toheat[toheat$species == 'goats','spec_tag'] <- 2
toheat[toheat$species == 'baboons','spec_tag'] <- 1

toheat$var_level <- factor(toheat$var, levels = c('mean_mean_nnd',
                                            "mean_sd_nnd",
                                            "sd_mean_nnd",
                                            "mean_pol",
                                            "sd_pol",
                                            "stdv_speed",
                                            "mean_sd_front",
                                            "mean_shape",
                                            "sd_shape"
))

levels(toheat$var_level) <- 1:9

pal <- wesanderson::wes_palette('Zissou1', 10, 'continuous')
Ap <- ggplot2::ggplot(toheat[toheat$var != 'mean_mean_bangl',],
                      ggplot2::aes( y = factor(species, levels = rev(c('fish', 'pigeons', 'goats', 'baboons'))),
                                    x = factor(var, levels = c( 'mean_mean_nnd',
                                                                  "mean_sd_nnd",
                                                                  "sd_mean_nnd",
                                                                  "mean_pol",
                                                                  "sd_pol",
                                                                  "stdv_speed",
                                                                  "mean_sd_front",
                                                                  "mean_shape",
                                                                  "sd_shape")),
                                    fill = as.factor(sl_bool))) +
  ggplot2::geom_tile(color ='black',alpha = 0.9)+
  ggplot2::scale_alpha_manual(breaks = c(FALSE, TRUE, NA),
                              values= c(0.95, 1, 0))+
  ggplot2::scale_fill_manual(breaks = c(TRUE, FALSE),
                             values = c('lightblue','grey60'),
                             labels = c('Yes (***)','No (ns)'))+
  ggplot2::labs(x = '', y = '', fill = 'Plateau:', alpha = 'Non stationarity')+
  ggplot2::geom_vline( ggplot2::aes(xintercept = 4.5), size = 1.5)+
  ggplot2::geom_vline( ggplot2::aes(xintercept =6.5), size = 1.5)+
  ggplot2::theme_minimal()+
  ggplot2::scale_y_discrete(labels = rev(c('Sticklebacks', 'Pigeons','Goats',  'Baboons')))+
  ggplot2::scale_x_discrete(labels =c( "NND",
                                       "Within-Group\nVar.NND",
                                       "Temporal\nVar.NND",
                                       "Polarization",
                                       "Temporal\nVar.Polarization",
                                       "Temporal\nVar.Speed",
                                       "Within-Group\nVar.Frontness",
                                       "Shape",
                                       "Temporal\nVar.Shape"))+
  ggplot2::geom_segment(data = transform(toheat[!(toheat$kpss_bool),],
                                         sample = as.numeric(var_level),
                                         species = as.numeric(spec_tag)),
                        ggplot2::aes(x = sample - .49, xend = sample + .49, y = species - .49, yend = species + .49),
                        color= 'black',
                        size=1) +
  ggplot2::coord_cartesian(ylim = c(1, 4.5), clip = "off") +
  ggplot2::annotate('text', x = 2.5,
                    y = 5, label = 'PC1', size = 5, family = 'Palatino Linotype' )+
  ggplot2::annotate('text' , x = 5.5,
                    y = 5, label = 'PC2', size = 5, family = 'Palatino Linotype' )+
  ggplot2::annotate('text' , x = 8,
                    y = 5, label = 'PC3', size = 5, family = 'Palatino Linotype' )+
  ggplot2::theme(legend.position = 'top',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 axis.title =  ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text =  ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text( size = 14, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 12, family = 'Palatino Linotype') )

# B. Changepoint ##########################################
Bp <- ggplot2::ggplot(toheat[!(toheat$var %in% c("mean_sd_front",
                                                 "mean_shape",
                                                 "sd_shape")) & toheat$species != 'pigeons',],
                ggplot2::aes(x = as.numeric(theta_s)/60,
                             y = factor(var, levels = rev(c('mean_mean_nnd',
                                                            "mean_sd_nnd",
                                                            "sd_mean_nnd",
                                                            "mean_pol",
                                                            "sd_pol",
                                                            "stdv_speed")
                                                          )),
                             color = sl_bool,
                             linetype = kpss_bool,
                             shape = sl_bool
                )) +
  ggplot2::scale_y_discrete(labels = c( "mean_mean_nnd" = "NND",
                                        "mean_sd_nnd" = "Within-Group Var. NND",
                                        "sd_mean_nnd" =  "Temporal Var. NND",
                                        "mean_pol" = "Polarization",
                                        "sd_pol" = "Temporal Var. Polarization",
                                        "stdv_speed"= "Temporal Var. Speed"))+
  ggplot2::geom_errorbar(ggplot2::aes(xmin = as.numeric(ci_ls)/60, xmax = as.numeric(ci_hs)/60 ),
                         width = .5, size = 1.2)+
  ggplot2::geom_point(size = 3, fill ='black')+
  ggplot2::facet_wrap( ~ factor(species, 
                                levels = c('fish', 'goats', 'baboons')),
                                scales = 'free_x',
                         labeller = ggplot2::as_labeller(c('baboons' = 'Baboons',
                                                'goats' = 'Goats',
                                                'fish' = 'Sticklebacks'))) +
  ggplot2::theme_bw()+
  ggplot2::labs(x = 'Time (mins)', y = '', color = 'Changepoint significance:', alpha = 'Signif:')+
  ggplot2::scale_color_manual(breaks = c(TRUE, FALSE),
                              values = c(pal[1], 'grey60')) +
  ggplot2::scale_linetype_manual(breaks = c(TRUE, FALSE),
                                 values = c('solid', 'dotted'),
                                 labels = c('***','ns'),
                                 guide = 'none') +
  ggplot2::scale_shape_manual(breaks = c(TRUE, FALSE, NA),
                              values = c(21, 1, 1),
                              guide = 'none') +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_line(color = 'grey'),
               legend.position = 'none',
               plot.title = ggplot2::element_blank(),
               strip.background = ggplot2::element_blank(),
               strip.placement = 'outside',
               strip.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype', hjust = 0.5),
               legend.text = ggplot2::element_text( color = 'black',size = 11, family = 'Palatino Linotype', hjust = 0.5),
               axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
               axis.text = ggplot2::element_text( color = 'black', size = 12, family = 'Palatino Linotype')
)

### //////////////////////////////////////////
# Metrics data per event per species
df_b <- read.csv('../data/col_motion_metrics/metrics_per_event_baboons.csv')
df_f <- read.csv('../data/col_motion_metrics/metrics_per_event_fish.csv')
df_g <- read.csv('../data/col_motion_metrics/metrics_per_event_goats.csv')
df_p <- read.csv('../data/col_motion_metrics/metrics_per_event_pigeons.csv')

# C.  ##########################################

toheat$ci_ls <- as.numeric(toheat$ci_ls)
toheat$ci_hs <- as.numeric(toheat$ci_hs)
spline_int <- as.data.frame(spline(cumsum(df_f$event_dur_s)/60, dplyr::cummean(df_f$mean_sd_nnd )))
Cp <- ggplot2::ggplot(df_f,
                    ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(mean_sd_nnd),  group = 1 )) +
  ggplot2::geom_line(data = spline_int, 
                     ggplot2::aes(x = x, y = y), 
                     size = 1,
                     color = 'black')+
  ggplot2::geom_rect(xmin = as.numeric(toheat[toheat$species == 'fish' & toheat$var == 'mean_sd_nnd', 'ci_ls'])/60,
                     xmax = as.numeric(toheat[toheat$species == 'fish' & toheat$var == 'mean_sd_nnd', 'ci_hs'])/60,
                     ymin=-Inf, ymax=Inf,
                     fill = 'lightblue', alpha = 0.01)+
  ggplot2::geom_vline(xintercept = as.numeric(toheat[toheat$species == 'fish' & toheat$var == 'mean_sd_nnd', 'theta_s'])/60,
                      color = pal[1], size = 1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
               y = 'Within-group\n Var. NND') +
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# D.  ##########################################

spline_int <- as.data.frame(spline( cumsum(df_g$event_dur_s)/60, dplyr::cummean(df_g$mean_pol )))
Dp <- ggplot2::ggplot(df_g,
                       ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(mean_pol),  group = 1 )) +
  ggplot2::geom_line(data = spline_int, ggplot2::aes(x = x, y = y), size = 1, color = 'black')+

  ggplot2::geom_vline(xintercept = toheat[toheat$species == 'goats' & toheat$var == 'mean_pol', 'theta_s']/60,
                      color = pal[1], size = 1.5)+
  ggplot2::geom_rect(xmin = toheat[toheat$species == 'goats' & toheat$var == 'mean_pol', 'ci_ls']/60,
                     xmax = toheat[toheat$species == 'goats' & toheat$var == 'mean_pol', 'ci_hs']/60,
                     ymin=-Inf, ymax=Inf,
                     fill = 'lightblue', alpha = 0.01)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
                y = 'Polarization') +
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# E.  ##########################################

spline_int <- as.data.frame(spline( cumsum(df_f$event_dur_s)/60, dplyr::cummean(df_f$stdv_speed )))
Ep <- ggplot2::ggplot(df_f,
                       ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(stdv_speed ),  group = 1 )
) +
  ggplot2::geom_line(data = spline_int, ggplot2::aes(x = x, y = y), size = 1, color = 'black')+
  ggplot2::geom_rect(xmin = toheat[toheat$species == 'fish' & toheat$var == 'stdv_speed', 'ci_ls']/60,
                     xmax = toheat[toheat$species == 'fish' & toheat$var == 'stdv_speed', 'ci_hs']/60,
                     ymin=-Inf, ymax=Inf,
                     fill = 'lightblue', alpha = 0.01)+
  ggplot2::geom_vline(xintercept = toheat[toheat$species == 'fish' & toheat$var == 'stdv_speed', 'theta_s']/60,
                      color =pal[1], size = 1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
                y = 'Temporal\nVar. Speed') +

  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# F.  ##########################################

spline_int <- as.data.frame(spline( cumsum(df_b$event_dur_s)/60, dplyr::cummean(df_b$mean_shape )))
Fp <- ggplot2::ggplot(df_b,
                       ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(mean_shape ),  group = 1 )
) +
  ggplot2::geom_line(size = 1, color = 'black') +
  ggplot2::geom_rect(xmin = toheat[toheat$species == 'baboons' & toheat$var == 'mean_shape', 'ci_ls']/60,
                     xmax = toheat[toheat$species == 'baboons' & toheat$var == 'mean_shape', 'ci_hs']/60,
                     ymin=-Inf, ymax=Inf,
                     fill = 'lightblue', alpha = 0.01)+
  ggplot2::geom_vline(xintercept = toheat[toheat$species == 'baboons' & toheat$var == 'mean_mean_nnd', 'theta_s']/60,
                      color = pal[1], size = 1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
                y = 'Shape (rads)') +

  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# G.  ##########################################

spline_int <- as.data.frame(spline( cumsum(df_p$event_dur_s)/60, dplyr::cummean(df_p$sd_shape )))
Gp <- ggplot2::ggplot(df_p,
                       ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(sd_shape ), group = 1 )) +
  ggplot2::geom_line(data = spline_int, ggplot2::aes(x = x, y = y), size = 1, color = 'black')+
  ggplot2::geom_rect(xmin = toheat[toheat$species == 'pigeons' & toheat$var == 'sd_shape', 'ci_ls']/60,
                     xmax = toheat[toheat$species == 'pigeons' & toheat$var == 'sd_shape', 'ci_hs']/60,
                     ymin = -Inf, ymax = Inf,
                     fill = 'grey60', alpha = 0.01)+
  ggplot2::geom_vline(xintercept = toheat[toheat$species == 'pigeons' & toheat$var == 'sd_pol', 'theta_s']/60,
                      color = 'grey60', size = 1.5, linetype = 'dotted')+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
                y = 'Temporal\nVar. Shape') +
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# H.  ##########################################

spline_int <- as.data.frame(spline( cumsum(df_g$event_dur_s)/60, dplyr::cummean(df_g$mean_mean_nnd )))
Hp <- ggplot2::ggplot(df_g,
                       ggplot2::aes(x = cumsum(event_dur_s)/60, y = dplyr::cummean(mean_mean_nnd),  group = 1)) +
  ggplot2::geom_line(data = spline_int, ggplot2::aes(x = x, y = y), size = 1, color = 'black')+
  ggplot2::geom_vline(xintercept = toheat[toheat$species == 'goats' & toheat$var == 'mean_mean_nnd', 'theta_s']/60,
                      color = 'grey60', size = 1.5, linetype = 'dotted')+
  ggplot2::geom_rect(xmin = toheat[toheat$species == 'goats' & toheat$var == 'mean_mean_nnd', 'ci_ls']/60,
                     xmax = toheat[toheat$species == 'goats' & toheat$var == 'mean_mean_nnd', 'ci_hs']/60,
                     ymin=-Inf, ymax=Inf,
                     fill = 'lightblue', alpha = 0.01)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = 'Time (min)',
                y = 'NND (m)') +
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black', size = 18, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(color = 'black', size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( color = 'black',size = 14, family = 'Palatino Linotype') )

# C-H. Lines multiplot  ##########################################

lineplots <- cowplot::plot_grid(Cp + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                   Dp + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                   Gp + ggplot2::theme(axis.title.x = ggplot2::element_blank()), 
                   Ep,
                   Fp,
                   Hp,
                   labels = c('C', 'D', 'E', 'F', 'G', 'H'))

# Fig 3  ##########################################

Fig3 <- cowplot::plot_grid(Ap, Bp, lineplots, nrow = 3, rel_heights = c(0.8, 1.1, 1.1), labels = c('A', 'B', ''))

# ggplot2::ggsave(plot = Fig3,
#        filename = 'Fig3.png',
#        dpi = 300,
#        height = 12,
#        width = 11
#        )

