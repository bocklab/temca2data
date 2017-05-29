
library(xlsx)
library(magrittr)
library(reshape2)
library(dplyr)
library(ggplot2)

# bad - no need to use xlsx
sheet2 = read.xlsx("/Users/zhengz11/myscripts/FAFB2017_paper/data/PNcountsPerGlom.xlsx", 1) %>%
  .[,-c(5,6)] 

t1 = sheet2[,c(1,2,4)] %>% 
  setNames(c("Glomerulus", "LM", "EM")) %>% 
  melt(id.vars="Glomerulus", variable.name="modality", value.name="PN_counts")

t2 = sheet2[,c(1,3)] %>% 
  setNames(c("Glomerulus", "LM")) %>%
  mutate("EM"=0) %>% 
  melt(id.vars="Glomerulus", variable.name="modality", value.name="SD")

t3 = inner_join(t1,t2)

# y=PN_counts, ymin=PN_counts-SD, ymax=PN_counts+SD
p <- ggplot(t3, aes(x=Glomerulus, y=PN_counts, fill=modality)) +
  geom_errorbar(aes(ymin=PN_counts-SD, ymax=PN_counts+SD, alpha=modality), position="dodge", width=0.5) +
  geom_bar(position="dodge", stat="identity", width = 0.5) +
  scale_y_continuous(breaks=scales::pretty_breaks(10), expand=c(0,0), limits = c(0,10)) +
  xlab("glomeruli") +
  ylab("Number of PNs") +
  scale_alpha_manual(values = c("LM"=0.6, "EM"=0), guide='none') +
  theme(legend.title = element_blank(), 
        text=element_text(size=36), 
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.ticks.x = element_blank(),
        legend.position = c(.95, .9),
        legend.text=element_text(size=20),
        axis.text.x=element_text(angle=90, vjust = 0.25),
        plot.margin=unit(c(0.8,.8,.8,.8),"cm"))
p

# ggsave("170518-pn_counts_per_glom_EMvsLM_vText.pdf", scale=1.2, width = 20, height = 6)
