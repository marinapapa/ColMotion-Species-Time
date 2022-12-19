##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 1
## Author: Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Dynamics of collective motion across time and species
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## For exact replication of the figures the font Palatino Linotype is needed
#extrafont::loadfonts('win')

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# A - Polarization vs speed heatmaps

pol_speed_data <- read.csv('../data/pol_speed_data.csv')
fdev <- pol_speed_data[pol_speed_data$species == 'fish',]
pdev <- pol_speed_data[pol_speed_data$species == 'pigeons',]
gdev <- pol_speed_data[pol_speed_data$species == 'goats',]
bdev <- pol_speed_data[pol_speed_data$species == 'baboons',]
rm(pol_speed_data)

# colorscale
pal <- wesanderson::wes_palette('Zissou1', 10, 'continuous')

A1p <- ggplot2::ggplot(fdev, ggplot2::aes(x = speed_av, y = pol_av))+
  ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = stat(density)), contour = FALSE) +
  ggplot2::geom_hline(yintercept = quantile(fdev$pol_av, 0.5, na.rm = T), color = 'black',linetype="dotted", size = 1 ) +
  ggplot2::geom_vline(xintercept = quantile(fdev$speed_av, 0.5, na.rm = T), color = 'black', linetype="dotted", size = 1) +
  ggplot2::scale_fill_gradientn(colors = c('deepskyblue4','white', 'yellow', pal[10], 'darkred')) +
  ggplot2::labs(x = 'Speed (m/s)', y = 'Polarization', title = 'Fish', fill = 'Density') +
  ggplot2::scale_x_continuous(
    limits = c(0, 2),
    expand = c(0,0))+
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    expand = c(0,0))+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Palatino Linotype'),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.title.x = ggplot2::element_blank()
  )

A2p <- ggplot2::ggplot(pdev, ggplot2::aes(x = speed_av, y = pol_av))+
  ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = stat(density)), contour = FALSE) +
  ggplot2::geom_hline(yintercept = quantile(pdev$pol_av, 0.05, na.rm = T), color = 'black',linetype="dotted", size = 1 ) +
  ggplot2::geom_vline(xintercept = quantile(pdev$speed_av, 0.05, na.rm = T), color = 'black', linetype="dotted", size = 1) +
  ggplot2::scale_fill_gradientn(colors = c('deepskyblue4','white', 'yellow', 'darkred')) +
  ggplot2::labs(x = 'Speed (m/s)', y = 'Polarization', title = 'Pigeons',fill = 'Density') +
  ggplot2::scale_x_continuous(
    limits = c(10, 26),
    expand = c(0, 0))+
  ggplot2::scale_y_continuous(
    limits = c(0.5, 1.1),
    expand = c(0, 0))+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title =  ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.title = ggplot2::element_blank()
  )

A3p <- ggplot2::ggplot(gdev, ggplot2::aes(x = speed_av, y = pol_av))+
  ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = stat(density)), contour = FALSE) +
  ggplot2::geom_hline(yintercept = quantile(gdev$pol_av, 0.75, na.rm = T), color = 'black',linetype="dotted", size = 1 ) +
  ggplot2::geom_vline(xintercept = quantile(gdev$speed_av, 0.75, na.rm = T), color = 'black', linetype="dotted", size = 1) +
  ggplot2::scale_fill_gradientn(colors = c('deepskyblue4','white', 'yellow', pal[10], 'darkred')) +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0))+
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0))+
  ggplot2::labs(x = 'Speed (m/s)', y = 'Polarization', title = 'Goats', fill = 'Density') +
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title =  ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Palatino Linotype')
  )


A4p <- ggplot2::ggplot(bdev, ggplot2::aes(x = speed_av, y = pol_av))+
  ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = stat(density)), contour = FALSE) +
  ggplot2::geom_hline(yintercept = quantile(bdev$pol_av, 0.9, na.rm = T), color = 'black',linetype="dotted", size = 1 ) +
  ggplot2::geom_vline(xintercept = quantile(bdev$speed_av, 0.9, na.rm = T), color = 'black', linetype="dotted", size = 1) +
  ggplot2::scale_fill_gradientn(colors = c('deepskyblue4','white', 'yellow', pal[10], 'darkred')) +
  ggplot2::labs(x = 'Speed (m/s)', y = 'Polarization', title = 'Baboons', fill = 'Density') +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0))+
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0))+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'none',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Palatino Linotype'),
                 axis.text.y = ggplot2::element_blank()
  )

### To add the species icons in R, please uncomment the following lines and adjust the icons positions to the size of your plot. Please contact the authors for the exact icons.
# A1p <- cowplot::ggdraw() +
#   cowplot::draw_plot(A1p)+
#   cowplot::draw_image(file.path('../icons/', "stickleback.png"),   x = 0.38, y = -0.33, scale = .2)
#
# A2p <- cowplot::ggdraw() +
#   cowplot::draw_plot(A2p)+
#   cowplot::draw_image(file.path('../icons/', "pigeon.png"),  x = 0.38, y = -0.3, scale = .2)
#
# A3p  <- cowplot::ggdraw() +
#   cowplot::draw_plot(A3p)+
#   cowplot::draw_image(file.path('../icons/', "goat.png"),  x = 0.35, y = -0.25, scale = .2)
#
# A4p <- cowplot::ggdraw() +
#   cowplot::draw_plot(A4p)+
#   cowplot::draw_image(file.path('../icons/', "baboon.png"),   x = 0.35, y = -0.25, scale = .2)

Ap <- cowplot::plot_grid(A1p,
                          A2p,
                          A3p,
                          A4p,
                          ncol = 2
)

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# B - Proportion of time in collective motion

all_events_durs <- read.csv('../data/all_events_durs.csv')
fdev <- all_events_durs[all_events_durs$species == 'fish',]
pdev <- all_events_durs[all_events_durs$species == 'pigeons',]
gdev <- all_events_durs[all_events_durs$species == 'goats',]
bdev <- all_events_durs[all_events_durs$species == 'baboons',]

vtimes <- c(max(pdev$cdur)/(pdev$tot_dur[1] * 0.2),
            max(fdev$cdur)/(fdev$tot_dur[1]  * 0.02),
            max(gdev$cdur)/(gdev$tot_dur[1] ),
            max(bdev$cdur)/(bdev$tot_dur[1] ))
vsps <- 1:4

Bp <- ggplot2::ggplot(data.frame(species = factor(vsps, levels = c('1', '2', '3', '4')), perc = vtimes),
                        ggplot2::aes(x = species, y = perc, label = round(perc), fill = as.factor(species))) +
  ggplot2::geom_col(ggplot2::aes(x = as.numeric(species), y = perc), color = 'black', size = 1,group = 1)+
  ggplot2::geom_point(color = 'black',
                      size = 4,
                      shape = 21)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = '',
                y = 'Time proportion in\n collective motion') +
  ggplot2::scale_x_discrete(labels = c('Pigeons', 'Sticklebacks', 'Goats', 'Baboons'))+
  ggplot2::scale_fill_manual(values = rev(c(pal[1], pal[4], pal[8], pal[10]))) +
  ggplot2::theme(legend.position = 'none',
                 plot.title.position = 'panel',
                 panel.grid = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(color = 'black',  size = 12, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text(color = 'black' ,size = 10, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Palatino Linotype'),
                 axis.text.x = ggplot2::element_text(color = 'black' ,size = 12, family = 'Palatino Linotype')
)

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Trajectories

##///////////
# C - Fish

ftimeser <- read.csv('../data/trajectories_eg/traj_eg_fish.csv')
Cp <- ggplot2::ggplot(ftimeser, ggplot2::aes(x = posx, y = posy, color = as.factor(id),
                                             fill = as.factor(id), alpha = time)) +
  ggplot2::geom_point(size = 2, shape = 16)+
  ggplot2::geom_segment(ggplot2::aes(x = 100, xend = 200, y = 400,  yend = 400, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(x =  100, xend =  100, y = 390,  yend = 410, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(x = 200, xend = 200, y = 390,  yend = 410, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_text(ggplot2::aes(x = 150, y = 400, label = '10 cm'), color = 'black',
                     family = 'Palatino Linotype', vjust = -0.5, size = 5) +
  ggplot2::scale_x_continuous(
    limits = c(20, 790),
    expand = c(0,0))+
  ggplot2::scale_y_continuous(
    limits = c(-50, 450),
    expand = c(0,0))+
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = c(pal[1], pal[4], pal[6], pal[8], pal[10])) +
  ggplot2::coord_equal()+
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text( size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype') 
                 )


##\\\\\\\\\\\\\\\\\\\\
# D - Pigeons

ptimeser <- read.csv('../data/trajectories_eg/traj_eg_pigeons.csv')
scale_dist <- raster::pointDistance(c(-0.599, 51.3755 ), c(-0.5985, 51.3755), lonlat = TRUE)
pal <- wesanderson::wes_palette('Zissou1', 10, 'continuous')

Dp <- ggplot2::ggplot(ptimeser, ggplot2::aes(x = posy, y = posx, color = as.factor(id),
                                           fill =  as.factor(id), alpha = time)) +
  ggplot2::geom_point(size = 2, shape = 16)+
  ggplot2::geom_segment(ggplot2::aes(y = -0.5970, yend = -0.5970, x = 51.3715,  xend = 51.3725, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(y = -0.5971, yend =  -0.5969, x = 51.3715,  xend = 51.3715, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(y = -0.5971, yend = -0.5969, x = 51.3725,  xend = 51.3725, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_text(ggplot2::aes(y =  -0.5970, x = 51.372, label = '35 m'), color = 'black', family = 'Palatino Linotype',
                     vjust = -0.5, size = 5) +
  ggplot2::scale_y_continuous(
    limits = c(-0.5995, -0.5963),
    expand = c(0,0))+
  ggplot2::scale_x_continuous(
    limits = c(51.371, 51.376),
    expand = c(0,0))+
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = c(pal[1], pal[3], pal[5], pal[8], pal[10],
                                         pal[2],pal[7], pal[9])) +
  ggplot2::coord_equal()+
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text( size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype') )


##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# E - Goats

gtimeser <- read.csv('../data/trajectories_eg/traj_eg_goats.csv')
pal <- wesanderson::wes_palette('Zissou1', 10, 'continuous')

Ep <- ggplot2::ggplot(gtimeser, ggplot2::aes(x = -lon, y = lat, color = as.factor(id),
                                               fill = as.factor(id), alpha = time)) +
  ggplot2::geom_point(size = 2, shape = 16)+
  ggplot2::geom_segment(ggplot2::aes(x = -15.7655, xend = -15.76575, y = -22.3782,  yend = -22.3782, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(x =  -15.7655, xend =  -15.7655, y = -22.37822,  yend = -22.37818, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(x = -15.76575, xend = -15.76575, y = -22.37822,  yend = -22.37818, linetype = 'solid'),inherit.aes = FALSE) +
  ggplot2::geom_text(ggplot2::aes(x = - 15.765626, y = -22.3782, label = '20 m'), color = 'black',
                     family = 'Palatino Linotype', vjust = -0.5, size = 5) +
  ggplot2::scale_x_continuous(
    limits = c( -15.76588, -15.76425),
    expand = c(0, 0))+
  ggplot2::scale_y_continuous(
    limits = c(-22.3787, -22.3779),
    expand = c(0, 0))+
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = c(pal[1], pal[3], pal[5], pal[8], pal[10],
                                         pal[2], pal[7], pal[9], pal[4], pal[6])) +
  ggplot2::coord_equal()+
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_text(size = 22, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text(size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text(size = 14, family = 'Palatino Linotype') 
                 )

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# F - Baboons

btimeser <- read.csv('../data/trajectories_eg/traj_eg_baboons.csv')
pal <- wesanderson::wes_palette('Zissou1', 20, 'continuous')
scale_dist <- raster::pointDistance(c(811150, 6214250 ), c(811200, 6214250), lonlat  = FALSE)

Fp <- ggplot2::ggplot(btimeser, ggplot2::aes(y = lon, x = lat, color = as.factor(id),
                                               fill = as.factor(id), alpha = time)) +
  ggplot2::geom_point(size = 2, shape = 16)+
  ggplot2::geom_segment(ggplot2::aes(y = 811298, yend = 811302, x = 6214100,  xend = 6214100, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(y =  811298, yend =  811302, x = 6214050,  xend = 6214050, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_segment(ggplot2::aes(y = 811300, yend = 811300, x = 6214050,  xend = 6214100, linetype = 'solid'), inherit.aes = FALSE) +
  ggplot2::geom_text(ggplot2::aes(y = 811300, x = 6214075, label = '50 m'), color = 'black',family = 'Palatino Linotype',
                     vjust = -0.5, size = 5) +
  ggplot2::scale_y_continuous(
     limits = c(811128, 811326),
     expand = c(0, 0))+
  ggplot2::theme_bw() +
  ggplot2::coord_equal()+
  ggplot2::scale_color_manual(values = c(pal[16], pal[12], pal[19],pal[7], pal[9], pal[10], pal[19],
                                         pal[3], pal[14], pal[20], pal[18], pal[1], pal[5])) +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text( size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype') )


##////////////
## Multiplot

### To add the species icons in R, please uncomment the following lines and adjust the icons positions to the size of your plot. Please contact the authors for the exact icons. 
# Cp_img <- cowplot::ggdraw() +
#   cowplot::draw_plot(Cp)+
#   cowplot::draw_image(file.path('../icons/', "stickleback.png"),  x = 0.41, y = -0.42, scale = .1)
#
# Dp_img <- cowplot::ggdraw() +
#   cowplot::draw_plot(Dp)+
#   cowplot::draw_image(file.path('../icons/', "pigeon.png"),  x = 0.41, y = -0.33, scale = .1)
#
# Ep_img <- cowplot::ggdraw() +
#   cowplot::draw_plot(Ep)+
#   cowplot::draw_image(file.path('../icons/', "goat.png"),  x = 0.41, y = -0.28, scale = .1)
#
# Fp_img <- cowplot::ggdraw() +
#   cowplot::draw_plot(Fp)+
#   cowplot::draw_image(file.path('../icons/', "baboon.png"),  x = 0.41, y =-0.33, scale = .1)
#
# CDEFp <- cowplot::plot_grid( Cp_img, Dp_img, Ep_img, Fp_img, nrow = 2, labels = c('C','D','E','F'))

CDEFp <- cowplot::plot_grid(Cp, Dp, Ep, Fp, nrow = 2)
# CDEFp <- cowplot::plot_grid( Cp, Dp, Ep, Fp, nrow = 2, labels = c('C', 'D','E','F'))
# The labels C to F in the published version of the plot have been added manually outside R.

ABp <- cowplot::plot_grid(Ap, Bp, nrow = 2, rel_heights = c(1.5, 0.5), labels = c('A', 'B'))
Fig1 <- cowplot::plot_grid(ABp, CDEFp, nrow = 1, rel_widths = c(0.6, 1.4))

# ggplot2::ggsave(Fig1, file = 'Fig1.png',
#        width = 14,
#        height = 8)

