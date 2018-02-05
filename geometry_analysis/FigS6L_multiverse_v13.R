
library(catmaid)
library(nat)
library(dplyr)
library(ggplot2)
library(mushroom)

# source(su)
#  id                label
#  53  multiverse2 seed AJ
#  58 multiverse2 seed IJA
#  63  multiverse2 seed LK

# writeWebGL(filename = "170830-multiverse_v7_overlap.html", width = 1000, height = 800)

# make soma start in the same place

p.nhop = 2;    # how many nodes away from central node to consider during rescop procedure.
p.close_thresh = 800; # How close counts as close for nodes
p.rs_dist = 80

# gl = read.neuron.catmaid(5117024, conn=fafb_conn) %>% resample(p.rs_dist)
# gl_nr = read.neuron.catmaid(5117024, conn=fafb_conn)
# nl = read.neurons.catmaid("annotation:^multiverse_teams$", conn=fafb_conn) %>% nlapply(resample, p.rs_dist)

get_votes <- function(dn, qn, close_thresh=p.close_thresh) {
  require(igraph)
  nn_dist = nabor::knn(data=xyzmatrix(dn), query=xyzmatrix(qn), k=1)
  ng = as.ngraph(qn)
  t1 = dfs(ng, root=which(qn$d$Parent == -1), neimode='out', unreachable=FALSE)$order
  counter = c()
  votes = c()
  for (j in t1) {
    t2 = ego(ng, mode = "all", nodes = t1[[j]])
    t_thresh = nn_dist$nn.dists[unlist(t2)] < close_thresh
    counter[[j]] = t_thresh %>% any %>% as.numeric
    votes[[j]] = t_thresh %>% all %>% as.numeric
  }
  list('counter'=counter, 'votes'=votes)
}

get_catmaid_link <- function(xyz) {
  sprintf('https://neuropil.janelia.org/tracing/fafb/v13/?pid=1&xp=%s&yp=%s&zp=%s&tool=tracingtool&sid0=5&s0=1',
          xyz[[1]], xyz[[2]], xyz[[3]])
}

get_fragment_parents <- function(n, subset_points) {
  # for example, subset_points = !(counter %>% as.logical)
  n_diff = subset(n, subset_points)
  n_diff$d[which(n_diff$d$Parent == -1),'PointNo']
}

# put together a table of team names, CATMAID links
results = c()
for (i in seq_along(nl)) {
  n_diff = subset(gl, !(discrp[[i]]$counter %>% as.logical))
  xyzd = n_diff$d[which(n_diff$d$Parent == -1), c("X","Y","Z")]
  t1 = c()
  for (row in 1:nrow(xyzd)) {
    t1[[row]] = get_catmaid_link(xyzd[row,])
  }
  results[[i]] = data.frame(t1) %>% setNames("link") %>% mutate("skid"=names(nl)[[i]])
}

path_between <- function(n, starts, ends) {
  t1 = get_tagged_node(n, starts, "indices")
  t2 = get_tagged_node(n, ends, "indices")
  shortest_paths(as.ngraph(n), from = t1, to = t2, mode='all', output = "vpath")
}

discrp = lapply(nl, get_votes, gl)
data_tbl = bind_rows(results)

results = list()
lens = list()
for (i in seq_along(nl)) {
  n_diff = subset(gl, !(discrp[[i]]$counter %>% as.logical))
  n_comp = n_diff %>% as.ngraph %>% components
  
  frag_list = c()
  len_c = c()
  for (j in 1:n_comp$no) {
    t1 = n_diff$d[which(n_comp$membership == j),'PointNo']
    frag_list[[j]] = t1
    len_c[[j]] = subset(gl, t1) %>% summary %>% .$cable.length
  }
  lens[[i]] = len_c
  results[[i]] = frag_list
#  points3d(gl$d[t11, c("X","Y","Z")], color = 'black', size=5)
}

get_len <- function(n) summary(n)$cable.length

# for 1st team neuron (skid - 5115189, name - "neuron 5115190")
# first 2 fragments should be combined:
# 1701 - 721649.6 212770.8 160677.4
# (1) 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711 1712 1713 1714 1715 1716 1717 1718 1719 1720 1721 1722 1723
# 1724 1725 1726 1727 1728 1729 1730 1731 1732 1733 1734 1735 1736 1737 1738 1739 1740 1741 1742 1743 1744 1745 1746
# 1747 1748 1749
# 1759 - 721650.5 212709.7 160894.1
# (2) 1759 1760 1761 1762
all_lens = lens
t1 = all_lens[[1]]
t1[[1]] = sum(t1[1:2])
t1 = t1[-c(2)]
all_lens[[1]] = t1

# for 2nd team neuron (skid - "5115222", )
# 3 fragments of 
# (3) 3245 3246 3247 3248 3249
# (4) 3275 3276 3277 3278 3279 3280 3281 3282 3283
# (5) 3304 3305 3306
# 3245     0 721843.2 222415.0 167825.0
# 3275     0 721462.0 222389.5 167509.2 
# 3304     0 721889.7 222280.7 167904.4
t2 = all_lens[[2]]
t2[[3]] = sum(t2[3:5])
t2 = t2[-c(4,5)]
all_lens[[2]] = t2

t1 = c()
for (i in seq_along(all_lens)) {
  t1[[i]] = all_lens[[i]] %>% data.frame %>% setNames("cable_length") %>% mutate(skid=names(nl)[i])
}
len_tbl = bind_rows(t1)

# plot len_tbl------

# 53  multiverse2 seed AJ - neuron 5115190 - yellow
# 58  multiverse2 seed IJA - neuron 5115223 - green
# 63  multiverse2 seed LK - neuron 5115229 - purple
len_tbl$skid = factor(len_tbl$skid, levels=c(5115222, 5115189, 5115228), ordered=TRUE)
len_tbl$cable_length = len_tbl$cable_length / 1e3

p <- ggplot(len_tbl, aes(x=skid, y=cable_length, color=skid)) +
  geom_point(size=5, position = position_jitter(0.15)) +
  scale_color_manual(values =c('green3', 'orange', 'purple')) +
  theme(legend.position = "none", panel.grid = element_blank(), 
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line()) +
  xlab("tracing teams") +
  ylab("cable length of missed branches (μm)") +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  scale_x_discrete(labels = c("5115222" = "team 1", "5115189" = "team 2", "5115228" = "team 3"))
p
# ggsave("170911-missed_branch_len_multiverse_v13.png")
# ggsave("171010-missed_branch_len_multiverse_v13.png")

arm1 = path_between(gl_nr, "1st_arm_starts", "1st_arm_ends")$vpath %>% unlist %>% {subset(gl_nr, .)}
arm2 = path_between(gl_nr, "2nd_arm_starts", "2nd_arm_ends")$vpath %>% unlist %>% {subset(gl_nr, .)}
arm3 = path_between(gl_nr, "3rd_arm_starts", "3rd_arm_ends")$vpath %>% unlist %>% {subset(gl_nr, .)}
sp = spine(gl_nr, UseStartPoint = TRUE, rval="neuron")

backbone_len = sapply(list(arm1, arm2, arm3, sp), get_len) %>% sum

total_len = get_len(gl_nr)

twig_len = total_len - backbone_len

len_summary = group_by(len_tbl, skid) %>% 
  summarize(error_counts=n(), sum_error_len=sum(cable_length)) %>%
  mutate(twig_len =twig_len  / 1e3, um_error=(twig_len - sum_error_len) / error_counts)

# skid error_counts sum_error_len twig_len um_error
# 1 5115222            8      10.79023  287.143 34.54410  # green
# 2 5115189            7       6.46501  287.143 40.09686  # yellow
# 3 5115228            7      14.64428  287.143 38.92839  # purple

# ------------------------
  # play with the 1st team to get fragment length-------
nl1_diff = subset(gl, !(discrp[[1]]$counter %>% as.logical))
t1_p = nl1_diff$d[which(nl1_diff$d$Parent == -1),'PointNo']
points3d(gl$d[t1_p, c("X","Y","Z")], color='black', size=8)

# play with components to get errors--------
nl2_diff = subset(gl, !(discrp[[2]]$counter %>% as.logical))
t2 = as.ngraph(nl2_diff)
components(t2)

nl3_diff = subset(gl, !(discrp[[3]]$counter %>% as.logical))
t3 = as.ngraph(nl3_diff)
components(t3)


# plotting the error regions--------------

n = discrp[[2]]

counter = n$counter
votes = n$votes
open3d()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
points3d(gl$d[!(counter %>% as.logical), c("X","Y","Z")], color='red', size=4)

n_diff = subset(gl, !(n$counter %>% as.logical))
t1_p = n_diff$d[which(n_diff$d$Parent == -1),'PointNo']
points3d(gl$d[t1_p, c("X","Y","Z")], color='black', size=8)
# writeWebGL(filename = "170907-GSvs5115189_descrepancy.html", width = 1000, height = 800)


# just testing false continuation comparing to goldstandard -----------
t1 = get_votes(gl, nl[[3]])
t_c = t1$counter
which(t_c == 0)
t2 = nl[[1]]
open3d()
plot3d(t2, col = 'yellow4', WithNodes = FALSE)
points3d(t2$d[!(t_c %>% as.logical), c("X","Y","Z")], color='red', size=4)


# test the length of each component-----

opend()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
points3d(gl$d[!(counter %>% as.logical), c("X","Y","Z")], color='red', size=4)


# use different colors for each fragment and combine some together--------
t11 = rainbow(12)
n = discrp[[3]]

counter = n$counter
votes = n$votes
open3d()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
frags = results[[3]]

for (j in 1:length(frags)) {
  points3d(gl$d[frags[[j]], c("X","Y","Z")], color=t11[j], size=5)
}

# plot and save the output of discrepant skeletons between each of the 3 teams to gold standard---------
open3d()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
t1_p = nl1_diff$d[which(nl1_diff$d$Parent == -1),'PointNo']
points3d(gl$d[t1_p, c("X","Y","Z")], color='black', size=8)
# writeWebGL(filename = "180129-GSvs5115189_descrepancy.html", width = 1000, height = 800)

open3d()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
n_diff = nl2_diff
t1_p = n_diff$d[which(n_diff$d$Parent == -1),'PointNo']
points3d(gl$d[t1_p, c("X","Y","Z")], color='black', size=8)
# writeWebGL(filename = "180129-GSvs5115222_descrepancy.html", width = 1000, height = 800)

open3d()
plot3d(gl, col = 'yellow4', WithNodes = FALSE)
n_diff = nl3_diff
t1_p = n_diff$d[which(n_diff$d$Parent == -1),'PointNo']
points3d(gl$d[t1_p, c("X","Y","Z")], color='black', size=8)
# writeWebGL(filename = "180129-GSvs5115228_descrepancy.html", width = 1000, height = 800)
