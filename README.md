# Collective motion across species and time

This repository contains the data and code accompanying the paper: 

*Papadopoulou M., Fürtbauer I., O’Bryan L., Garnier S., Georgopoulou D., Bracken A., Christensen C., and King A.J.
"Dynamics of collective motion across time and species". Philos. Trans. R. Soc. B https://doi.org/10.1098/rstb.2022-0068.R2*

The code provided is open source, but we kindly ask you to cite the above paper if you make use of it. 

## Data 
**(doi: 10.5281/zenodo.7457770)**

The data comprise analysed tragectories of four species: stickleback fish, homing pigeons, goats and chacma baboons. Specifically, they include:
1. *col_motion_metrics*: metrics of collective motion for each event of collective motion of all species. The columns include: the unique id of events with duration more than 15 seconds (*event*), the 9 metrics that composed the swarm space, the duration of each event in sampling steps (*event_dur*) and the duration in seconds (*event_dur_s*).
2. *relative_positions*: bearing angles (in rad) of all group members to their closest 4 neighbors (*rank* 1 to 4) during all events of collective motion of all species. Plotted in Figure 2.
3. *trajectories_eg*: example trajectories of events of collective motion of all species. Plotted in Figure 1.
4. *swarm_space*: the results of the swarm space analysis, specifically the PCA output (*pca_res.Rdata*), the coordinates of all events in the PC1-PC2 space (*pcs_data.csv*), and in the tSNE1-tSNE2 space (*tsne_data.csv*), with group size information included. Plotted in Figure 2.
5. *all_events_durs.csv*: the summary of events per species. Columns include: *ev_count* (number of events in a given day), *dur* (total duration of events in a day), *cdur* (cumulative duration of events over days), *tot_dur* (the total duration of events across all days per species). Plotted in Figure 1.
6. *pol_speed_data.csv*: smoothed polarization and speed of all groups/dates across species. Plotted in Figure 1.
7. *changepoints_stats.csv*: the results of our changepoint (segmented regression) and stationarity (kpss tests) analysis. Created by the *across_time_analysis.R* file. Includes columns: *var* (metric name), *theta_idx* (the point in the events timeseries that the changepoint is identified), *theta_s* (the changepoint in seconds), *sl* (the confidence level of the changepoint identification), *sl_bool* (whether the changepoint is significant), *ci_l* and *ci_h* (lower and upper confidence interval of changepoint), *ci_ls* and *ci_hs* (the confidence intervals in seconds), *boots_sd* (the standard deviation of the changepoint from the bootstrap analysis), *kpss_pvalue* (the p-value of the kpss test for stationarity), *kpss_bool* (whether a timeseries is identified as stationary or not). Plotted in Figure 3. 

The raw data are collected for the previous studies: 

- Sticklebacks: 

*Georgopoulou DG, King AJ, Brown RM, Fürtbauer I. 2022 Emergence and repeatability of leadership and coordinated motion in fish shoals. Behavioral Ecology 33, 47–54. https://doi.org/10.1093/beheco/arab108*
- Pigeons: 

*Sankey DWE, Storms RF, Musters RJ, Russell WT, Hemelrijk CK, Portugal SJ. (2021). "Absence of “selfish herd” dynamics in bird flocks under threat". Current Biology. https://doi.org/10.1016/j.cub.2021.05.009*

- Goats:

*Sankey DWE, O’Bryan LR, Garnier S, Cowlishaw G, Hopkins P, Holton M, Fürtbauer I, King AJ. 2021 Consensus of travel direction is achieved by simple copying, not voting, in free-ranging goats. Royal Society Open Science 8, rsos.201128, 201128. https://doi.org/10.1098/rsos.201128*

- Baboons:

*Bracken AM, Christensen C, O’Riain MJ, Fürtbauer I, King AJ. 2022 Flexible group cohesion and coordination, but robust leader–follower roles, in a wild social primate using urban space. Proceedings of the Royal Society B: Biological Sciences 289, 20212141. https://doi.org/10.1098/rspb.2021.2141* 


## Code

The code reproduces the main figures and across time analysis of the study.
All analysis is performed in _R_, version 4.1. Files:

- *Fig1.R*: reproduces Figure 1.
- *Fig2.R*: reproduces Figure 2.
- *Fig3.R*: reproduces Figure 3.
- *across_time_analysis.R*: performs the timeseries analysis on the metrics of collective motion across species (changepoint analysis, bootstrap, and kpss stationarity test), producing the *changepoints_stats.csv* file, plotted in Figure 3. 

Package dependencies:
_dplyr, tseries, lm.br, boot, stringr, reshape2, raster, ggplot2, cowplot, wesanderson, (extrafont)_

## Contact
* For any further information, email **Marina Papadopoulou** at: <m.papadopoulou.rug@gmail.com>
