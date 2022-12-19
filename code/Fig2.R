##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 2
## Author: Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Dynamics of collective motion across time and species
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## For exact replication of the figures the font Palatino Linotype is needed
#extrafont::loadfonts('win')

# A-C. PCA Components ###################################

load('../data/swarm_space/pca_res.Rdata')
pca_df <- read.csv('../data/swarm_space/pca_data.csv')

pca_df$gsize <- 1
pca_df[pca_df$group_size >= 10, 'gsize'] <- 2
pca_df[pca_df$group_size > 20, 'gsize'] <- 3

# colorscale
pal <- wesanderson::wes_palette('Zissou1', 10, 'continuous')

Ap <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2,
                                            fill = species,
                                            label = group_size,
                                            size = as.factor(gsize),
                                            shape = species)) +
  ggplot2::geom_point( color = 'black')+
  ggplot2::labs(fill = 'Species:', shape = 'Species:', y = 'PC2 - Plasticity',
                x = 'PC1 - Group cohesion & order',
                title = 'PCA space')+
  ggplot2::scale_fill_manual(values=c( pal[4], pal[10], pal[8],  pal[1]),
                             labels = c('Sticklebacks','Pigeons', 'Goats',  'Baboons' ),
                             breaks = c('fish', 'pigeons', 'goats', 'baboons'))+
  ggplot2::scale_shape_manual(values=c( 22, 24,23, 21),
                              labels = c('Sticklebacks','Pigeons', 'Goats',  'Baboons' ),
                              breaks = c('fish', 'pigeons', 'goats', 'baboons'))+
  ggplot2::scale_size_manual(values=c(4, 5.5, 7))+
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'top',
                 legend.direction = 'horizontal',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.title =  ggplot2::element_text(color = 'black', size = 14, family = 'Palatino Linotype'),
                 axis.text =   ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text( size = 14, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype') )+
  ggplot2::guides(size = 'none', fill = ggplot2::guide_legend(override.aes = list(size = 4)))


#####################################################
## PCA plots with metric axes
lam2 <- (my.pca$sdev[c(1, 3)]*sqrt(10))
len2 <- t(t(my.pca$rotation[, c(1, 3)]) * lam2)

Bp <- ggplot2::ggplot(pca_df) +
  ggplot2::geom_point(ggplot2::aes(x = PC1, y = PC3,
                                   fill = species,
                                   shape = species),
                      color = 'black',
                      size = 3,
                      alpha = 0.3) +
  ggplot2::labs(colour = 'Species:',
                shape = 'Species:',
                y = 'PC3 - Group structure',
                x = 'PC1 - Group cohesion & order') +
  ggplot2::scale_fill_manual(values=c(pal[1], pal[4], pal[8], pal[10]))+
  ggplot2::scale_shape_manual(values=c(21, 22, 23, 24))+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = len2['mean_mean_nnd', 1], yend = len2['mean_mean_nnd', 2] ),
                        color = 'grey5', size = 1, arrow = ggplot2::arrow())+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = len2['mean_pol', 1], yend = len2['mean_pol', 2] ),
                        color = 'grey10', size = 1, arrow = ggplot2::arrow())+  ggplot2::theme_bw() +
  ggplot2::geom_text(label = 'Polarization', x = len2['mean_pol', 1] + 1.5, y = len2['mean_pol', 2], size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_text(label = 'NND', x =len2['mean_mean_nnd', 1] - 1, y = len2['mean_mean_nnd', 2], size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = len2['mean_shape', 1], yend = len2['mean_shape', 2] ),
                        color = 'grey25', size = 1, arrow = ggplot2::arrow())+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = len2['mean_sd_front', 1], yend = len2['mean_sd_front', 2] ),
                        color = 'grey30', size = 1, arrow = ggplot2::arrow())+  ggplot2::theme_bw() +
  ggplot2::geom_text(label = 'Frontness\nVar.', x =len2['mean_sd_front', 1] - 1, y = len2['mean_sd_front', 2] - 0.2, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_text(label = 'Shape', x = len2['mean_shape', 1] + 0.5, y = len2['mean_shape', 2] - 0.5, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::theme_bw() +
  ggplot2::coord_equal(xlim = c(-6, 5), ylim = c(-3.5, 2.8))+
  ggplot2::theme(legend.position = 'none',
                 plot.title.position = 'panel',
                 text = ggplot2::element_text( size = 14, family = 'Palatino Linotype', hjust = 0.5),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text( size = 14, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 panel.grid.minor = ggplot2::element_blank()
  )


lam3 <- (my.pca$sdev[c(2, 3)]*sqrt(10))
len3 <- t(t(my.pca$rotation[, c(2, 3)]) * lam3)

Cp <- ggplot2::ggplot(pca_df) +
  ggplot2::geom_point(ggplot2::aes(y = PC3, x = PC2,
                                   fill = species,
                                   shape = species),
                      color = 'black',
                      size = 3,
                      alpha = 0.3)+
  ggplot2::labs(colour = 'Species:',
                shape = 'Species:',
                x = 'PC2 - Plasticity',
                y = 'PC3 - Group structure')+
  ggplot2::scale_fill_manual(values=c(pal[1], pal[4], pal[8], pal[10]))+
  ggplot2::scale_shape_manual(values=c(21, 22, 23, 24))+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, yend = len3['mean_shape', 2], xend = len3['mean_shape', 1] ),
                        color = 'grey25', size = 1, arrow = ggplot2::arrow())+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, yend = len3['mean_sd_front', 2], xend = len3['mean_sd_front', 1] ),
                        color = 'grey30', size = 1, arrow = ggplot2::arrow())+  ggplot2::theme_bw() +
  ggplot2::geom_text(label = 'Frontness\nVar.', y = len3['mean_sd_front', 2], x = len3['mean_sd_front', 1] + 1, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_text(label = 'Shape', y =len3['mean_shape', 2], x = len3['mean_shape', 1] - 0.2, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, yend = len3['sd_pol', 2], xend = len3['sd_pol', 1] ),
                        color = 'grey15', size = 1, arrow = ggplot2::arrow())+
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, yend = len3['stdv_speed', 2], xend = len3['stdv_speed', 1] ),
                        color = 'grey20', size = 1, arrow = ggplot2::arrow())+  ggplot2::theme_bw() +
  ggplot2::geom_text(label = 'Polarization\nVar.', y = len3['sd_pol', 2],x = len3['sd_pol', 1] - 1.7, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::geom_text(label = 'Speed Var.', y =len3['stdv_speed', 2] + 0.3, x = len3['stdv_speed', 1] - 1.7, size = 4,
                     vjust = 1,  family = 'Palatino Linotype')+
  ggplot2::theme_bw() +
  ggplot2::coord_equal(xlim = c(-6.5, 4.5),
                       ylim = c(-3.5, 3))+
  ggplot2::theme(legend.position = 'none',
                 plot.title.position = 'panel',
                 text = ggplot2::element_text( size = 14, family = 'Palatino Linotype', hjust = 0.5),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype', hjust = 0.5),
                 axis.title = ggplot2::element_text( size = 14, family = 'Palatino Linotype'),
                 axis.text =   ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 panel.grid.minor = ggplot2::element_blank()
  )


##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# D - tSNE

tsnedata <- read.csv('../data/swarm_space/tsne_data.csv')
tsnedata$gsize <- 1
tsnedata[tsnedata$group_size >= 10, 'gsize'] <- 2
tsnedata[tsnedata$group_size > 20, 'gsize'] <- 3

Dp <- ggplot2::ggplot(tsnedata,  ggplot2::aes(x = y, y = x,
                                              fill = species,
                                              size = as.factor(gsize),
                                              shape = species)) +
  ggplot2::geom_point(color = 'black')+
  ggplot2::labs(fill = 'Species:', shape = 'Species:',
                title = 'tSNE space', x = 't-SNE 1', y = 't-SNE 2')+
  ggplot2::scale_fill_manual(values=c(pal[1], pal[4], pal[8], pal[10]))+
  ggplot2::scale_shape_manual(values=c(21, 22, 23, 24))+
  ggplot2::scale_size_manual(values=c(3, 4, 5))+
  ggplot2::coord_equal()+
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.title =  ggplot2::element_text(color = 'black', size = 14, family = 'Palatino Linotype'),
                 axis.text =   ggplot2::element_text(color = 'black', size = 12, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text( size = 14, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text( size = 14, family = 'Palatino Linotype')
                 )+
  ggplot2::guides(size = 'none', fill = ggplot2::guide_legend(override.aes = list(size = 4)))

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Merge A-E

BCp <- cowplot::plot_grid(Bp, Cp, labels = c('B', 'C'))
ABCp <- cowplot::plot_grid(Ap, BCp, nrow = 2, labels = c('A', '', ''), rel_heights = c(0.7, 0.6))
ADp <- cowplot::plot_grid(ABCp, Dp, nrow = 2, rel_heights = c(1.1, 0.6), labels = c('', 'D'))

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# E-H - Rel position neighbors
palfordens <- c('#fef0d9',  '#fdcc8a',  pal[8], '#d7301f', '#942115', 'black')

# Fish
bangl_f <- read.csv('../data/relative_positions/b_angles_rank1to4_fish.csv')

Ep <- ggplot2::ggplot(bangl_f, ggplot2::aes(x = bangl, y = rank)) +
  ggplot2::stat_density_2d(
    geom = "tile",
    ggplot2::aes(fill = ..density..),
    n = c(40, 4),
    contour = F
  ) +
  ggplot2::geom_hline(yintercept = 1.5, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 2.55, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 3.5, color = 'black', size = 1.3) +
  ggplot2::labs(x = 'Bearing angle (rad)',
                y = 'Neighbor rank',
                title = 'Sticklebacks',
                fill = 'Density') +
  ggplot2::scale_fill_gradientn(colours = palfordens) +
  ggplot2::coord_polar(start = pi)+
  ggplot2::geom_point(x = 0, y = 0.5, size = 3, shape = 17 ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 axis.title.x = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text(size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text(size = 10, family = 'Palatino Linotype') )


# Pigeons
bangl_p <- read.csv('../data/relative_positions/b_angles_rank1to4_pigeons.csv')

Fp <- ggplot2::ggplot(bangl_p, ggplot2::aes(x = bangl, y = rank)) +
  ggplot2::stat_density_2d(
    geom = "tile",
    ggplot2::aes(fill = ..density..),
    n = c(40, 4),
    contour = F
  ) +
  ggplot2::geom_hline(yintercept = 1.5, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 2.55, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 3.5, color = 'black', size = 1.3) +
  ggplot2::labs(x = 'Bearing angle (rad)',
                y = 'Neighbor rank',
                title = 'Pigeons',
                fill = 'Density') +
  ggplot2::scale_fill_gradientn(colours = palfordens) +
  ggplot2::coord_polar(start = pi)+
  ggplot2::geom_point(x = 0, y = 0.5, size = 3, shape = 17 ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 axis.title.x = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text(size = 12, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text(size = 8, family = 'Palatino Linotype') )

# Goats
bangl_g <- read.csv('../data/relative_positions/b_angles_rank1to4_goats.csv')

Gp <- ggplot2::ggplot(bangl_g, ggplot2::aes(x = bangl, y = rank), color = 'black') +
  ggplot2::stat_density_2d(
    geom = "tile",
    ggplot2::aes(fill = ..density..),
    n = c(40, 4),
    contour = F
  ) +
  ggplot2::geom_hline(yintercept = 1.5, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 2.55, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 3.5, color = 'black', size = 1.3) +
  ggplot2::labs(x = 'Bearing angle (rad)',
                y = 'Neighbor rank',
                title = 'Goats',
                fill = 'Density') +
  ggplot2::scale_fill_gradientn(colours = palfordens) +
  ggplot2::coord_polar(start = pi)+
  ggplot2::geom_point(x = 0, y = 0.5, size = 3, shape = 17 ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 14, family = 'Palatino Linotype'),
                 legend.title = ggplot2::element_text(size = 16, family = 'Palatino Linotype'),
                 legend.text = ggplot2::element_text(size = 10, family = 'Palatino Linotype') )

# Baboons
bangl_b <- read.csv('../data/relative_positions/b_angles_rank1to4_baboons.csv')
Hp <- ggplot2::ggplot(bangl_b, ggplot2::aes(x = bangl, y = rank)) +
  ggplot2::stat_density_2d(
    geom = "tile",
    ggplot2::aes(fill = ..density..),
    n=c(40, 4),
    contour = F
  ) +
  ggplot2::geom_hline(yintercept = 1.5, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 2.55, color = 'black', size = 1.3) +
  ggplot2::geom_hline(yintercept = 3.5, color = 'black', size = 1.3) +
  ggplot2::labs(x = 'Bearing angle (rad)',
                y = 'Neighbor rank',
                title = 'Baboons',
                fill = 'Density') +
  ggplot2::scale_fill_gradientn(colours = palfordens) +
  ggplot2::coord_polar(start = pi)+
  ggplot2::geom_point(x = 0, y = 0.5, size = 3, shape = 17 ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 plot.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 14, color = 'black',  family = 'Palatino Linotype'),
                 axis.text = ggplot2::element_text(size = 14, color = 'black', family = 'Palatino Linotype')
 )


### To add the species icons in R, please uncomment the following lines and adjust the icons positions to the size of your plot
### In the published version of this plot, the icons have been manually added to the plot outside R.

# Ep <- cowplot::ggdraw() +
#   cowplot::draw_plot(Ep)+
#   cowplot::draw_image(file.path('../icons/', "stickleback.png"),  x = 0.4, y = -0.42, scale = .1)
#
# Fp <- cowplot::ggdraw() +
#   cowplot::draw_plot(Fp)+
#   cowplot::draw_image(file.path('../icons/', "pigeon.png"),  x = 0.4, y = -0.4, scale = .1)
#
# Gp <- cowplot::ggdraw() +
#   cowplot::draw_plot(Gp)+
#   cowplot::draw_image(file.path('../icons/', "goat.png"),  x = 0.4, y = -0.4, scale = .1)
#
# Hp <- cowplot::ggdraw() +
#   cowplot::draw_plot(Hp)+
#   cowplot::draw_image(file.path('../icons/', "baboon.png"),  x = 0.4, y = -0.4, scale = .1)

EFGH2p <- cowplot::plot_grid(Ep, Fp, Gp, Hp, ncol = 1,  labels = c('E', 'F', 'G', 'H'), rel_heights = c(1, 1, 1, 1.08))
Fig2 <- cowplot::plot_grid(ADp, EFGH2p, ncol = 2, rel_widths = c(1, 0.4))

# ggplot2::ggsave(Fig2, file = 'Fig2.png',
#                 width = 10.5,
#                 height = 9.7,
#                 dpi = 300)
