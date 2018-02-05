library(dplyr)
library(RSQLite)
library(reshape2)
library(ggplot2)
library(catmaid)

get_displacements <- function(n, n_sm) {
  nx = xyzmatrix(n)
  nsx = xyzmatrix(n_sm)
  r = sapply(seq_len(nrow(nx)), function(x) sqrt(sum((nx[x,] - nsx[x,])^2)))
  data.frame(r, n$d$Z/35) %>% setNames(c("distance","section_num"))
}

smooth_nl <- function(nl) nlapply(nl, smooth_neuron, method = "gauss", sigma=12000)

# kc_nl = read.neurons.catmaid("annotation:^fafb_ms_KCs$", conn=fafb_conn)
# apl_nl = read.neurons.catmaid(203840, conn=fafb_conn)
# pn_nl = read.neurons.catmaid("annotation:^fafb_ms_PNs$", conn=fafb_conn)

kc_sm_nl = nlapply(kc_nl, smooth_neuron, method = "gauss", sigma=12000)
apl_sm_nl = nlapply(apl_nl, smooth_neuron, method = "gauss", sigma=12000)
pn_sm_nl = nlapply(pn_nl, smooth_neuron, method = "gauss", sigma=12000)


# start processing neuronlist (nl)
# nl = kc_nl
# nl = apl_nl
nl = c(pn_nl, apl_nl, kc_nl)
nl_sm = smooth_nl(nl)

results = lapply(seq_along(nl), function(x) get_displacements(nl[[x]], nl_sm[[x]]))

# get the means of each section---------
data_tbl2 = bind_rows(results) %>%
  group_by(section_num) %>% 
  summarise(dist_mean=mean(distance))

t1 = data_tbl2$dist_mean
t2 = abs(t1[2:length(t1)] - t1[1:(length(t1)-1)]) / 1e3
t3 = cbind(t2, data_tbl2$section_num[2:length(t1)]) %>% data.frame %>% setNames(c("delta","section_num"))

t4 = seq_len(7062) %>% setdiff(t3$section_num %>% unique) %>% data.frame %>% setNames("section_num") %>% full_join(t3)

p <- ggplot(t4, aes(x=section_num, y=delta)) +
  geom_point(size=2.5, alpha=0.4) +
  scale_y_continuous(breaks = scales::pretty_breaks(12), limits = c(NA, 11), expand = c(0,0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(14), expand = c(0,0)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size=1.5)) +
  ylab("distance (um)") +
  xlab("section number")
p
# ggsave("171206-displacement_delta_12um_ms_all_cutbg_smaller.pdf", width=19.92, height=6)

median(t4$delta, na.rm=TRUE)
# median of all sections is 0.09 um

quantile(t4$delta, .95, na.rm=TRUE)
# 95% of sections are less than 0.57 um