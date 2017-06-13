
uPN_in_fc = xform_brain(uPN, sample=FAFB13, reference=FCWB)
fb_ca_coll = get_calyx_collaterals(uPN_in_fc)
fb_ca_coll[,'std_glom']=glom_data[fb_ca_coll[,'glomerulus']]
  # fb_tbl = summarize_pair_wise(fb_ca_coll, fb_std_gloms, 'FAFB', get_dist_summary)
load(paste0(getwd(), "/data/lm_coll_subset_170331.rda"))
  
lm_std_gloms = c(fc_std_gloms, gj_std_gloms)
  
fb_coll_tbl = summarize_pair_wise(fb_ca_coll, fb_std_gloms, 'FAFB', get_dist_summary)

fb_nblast_tbl = summarize_pair_wise(fb_ca_coll, fb_std_gloms, 'FAFB', get_nblast_score)
  
# lm_subset_170331 is a subset of LM PNs from sampling the whole population of LM PNs from both sources (flycircuit and Jefferis2007)
lm_coll_subset = lm_subset_170331
  
lm_coll_tbl_subset = summarize_pair_wise(lm_coll_subset, lm_std_gloms, 'LM', get_dist_summary)
lm_nblast_subset = summarize_pair_wise(lm_coll_subset, lm_std_gloms, 'LM', get_nblast_score)
  
emlm_tbl_subset = rbind(lm_coll_tbl_subset, fb_coll_tbl) %>% mutate(type=factor(.$type))
emlm_nblast_tbl = rbind(lm_nblast_subset, fb_nblast_tbl) %>% mutate(type=factor(.$type))
  
  # order glom by difference between LM and EM------
all_glom_rank = rank_glom_by_diff(emlm_tbl_subset) %>% 
  c(gsub("glomerulus ", "", fb_gloms_extra) %>% sort) %>% 
  print
  
nblast_glom_rank = rank_glom_by_nblast(emlm_nblast_tbl) %>%
  c(gsub("glomerulus ", "", fb_gloms_extra) %>% sort) %>% 
  print


# plotting----------
results = tidyr::complete(emlm_tbl_subset, type, groups)
results$type = factor(results$type, levels=all_glom_rank)
results = mutate(results, pseudo_mean = dist_mean)
p <- ggplot(results, aes(y=dist_mean, x=groups)) +
  geom_jitter(aes(color=groups), size = 2.8, width=.36) +
  facet_grid(. ~ type, switch = "x") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size=20),
        strip.background = element_rect(fill='white'),
        legend.title = element_blank(),
        text=element_text(size=20),
        legend.position = "right", 
        panel.spacing = unit(.2, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=8), limits = c(0,17), expand=c(0,0)) +
  scale_color_discrete(labels=c("EM", "LM")) +
  ylab("pair-wise mean distance") +
  xlab("glomeruli")
p
# ggsave("170418-EMvsLM_CAcoll_dist_scattered1_wide.png", scale=1.2, width=20, height=6)

# t test-------
# fb_std_gloms, fc_std_gloms, gj_std_gloms

get_nums <- function(group_name, gloms=NULL, tbl=results, stat_col='dist_mean') {
  if (is.null(gloms)) dist_n = filter(tbl, groups==group_name)[,stat_col]
  else dist_n = filter(tbl, groups==group_name, type %in% gloms)[,stat_col]
  unlist(dist_n) %>% unname
}

LMvsEM_t_test = t.test(get_nums('FAFB', intersect(fb_std_gloms, lm_std_gloms)), get_nums('LM'))

LMvsEM_nblast_t_test = t.test(get_nums('FAFB', intersect(fb_std_gloms, lm_std_gloms), emlm_nblast_tbl, 'nblast_mean_score'), 
                              get_nums('LM', tbl=emlm_nblast_tbl, stat_col='nblast_mean_score'))

# histogram------
data_tbl = emlm_tbl_subset
mean_tbl = filter(data_tbl, type %in% unname(glom_data[fb_gloms])) %>% 
  group_by(groups) %>% 
  summarize(m=mean(dist_mean), sd=sd(dist_mean))

idx = which(data_tbl$dist_mean > 10)
data_tbl[idx, 'dist_mean'] = rep(10.2, length(idx))

p <- ggplot(data_tbl, aes(x=dist_mean, fill=groups)) +
  stat_bin(breaks=seq(1, 10.5, 0.5), position="dodge") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), limit=c(0,22), expand=c(0,0)) +
  scale_x_continuous(breaks=seq(1, 10.5, 0.5), labels=c(seq(1, 10, 0.5), "> 10")) +
  scale_fill_discrete(labels=c('EM','LM')) +
  annotate("point", x=3.5, y=21.5, size=.5) +
  annotate("point", x=5.5, y=21.5, size=.5) +
  theme(legend.title = element_blank(),
        text=element_text(size=20),
        legend.position = 'right',
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line()) +
  ylab("counts") +
  xlab("mean distance")
p
# ggsave("170418-EMvsLM_CAcoll_dist_hist_wide.png", scale=1.2, width=20, height=6)

# nblast plotting-------
results = emlm_nblast_tbl
results = tidyr::complete(results, type, groups)
results$type = factor(results$type, levels=nblast_glom_rank)
p <- ggplot(results, aes(y=nblast_mean_score, x=groups)) +
  geom_jitter(aes(color=groups), size = 2.8, width=.36) +
  facet_grid(. ~ type, switch = "x") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      #  strip.text.x=element_text(size=12),
        legend.title = element_blank(),
        text=element_text(size=20),
        legend.position = "right",
        strip.background = element_rect(fill='white'),
        panel.spacing = unit(.2, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_color_discrete(labels=c('EM','LM')) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=10), limit=c(-0.2, .9), expand=c(0,0)) +
  ylab("pair-wise NBLAST scores \n(higher scores => more similar)") +
  xlab("glomeruli")
p
# ggsave("170418-EMvsLM_CAcoll_nblast_scattered_annotated_wide.png", scale=1.2, width=20, height=6)

# histogram of nblast scores------
data_tbl = emlm_nblast_tbl
mean_nblast_tbl = filter(data_tbl, type %in% unname(glom_data[fb_gloms])) %>% 
  group_by(groups) %>% 
  summarize(m=mean(nblast_mean_score), sd=sd(nblast_mean_score))

idx = which(data_tbl$nblast_mean_score < -0.05)
data_tbl[idx, 'nblast_mean_score'] = rep(-0.055, length(idx))

nb_break = seq(-0.1, 0.9, .05)

p <- ggplot(data_tbl, aes(x=nblast_mean_score, fill=groups)) +
   stat_bin(breaks=seq(-0.1, 0.9, .05), position='dodge') +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), limit=c(0,23), expand=c(0,0)) +
  scale_x_continuous(breaks=seq(-0.1, 0.9, .05), labels=c("< -0.05", seq(-0.05, 0.9, .05)), expand=c(0,0)) +
  scale_fill_discrete(labels=c('EM','LM')) +
  annotate("point", x=.55, y=22.5, size=.5) +
  annotate("point", x=.34, y=22.5, size=.5) +
  theme(legend.title = element_blank(), 
        text=element_text(size=20),
        legend.position = 'right',
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line()) +
  ylab("counts") +
  xlab("NBLAST score")
p
# ggsave("170418-EMvsLM_CAcoll_nblat_hist_wide.png", scale=1.2, width=20, height=6)

