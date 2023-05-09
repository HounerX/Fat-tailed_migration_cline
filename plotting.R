library(ggplot2)

bound=10


simulated_s1<-read.csv("data/s1_4_03_simulated_b10.txt",header = TRUE)
simulated_s1<-simulated_s1[1:999,]

approximation_s1<-read.csv("data/s1_approx_4_03_b10.txt",header = FALSE)
approximation_s1<-approximation_s1[1:999,]

simvsapprox_s1<-data.frame(x_cords=simulated_s1$x_cords,simulated=simulated_s1$allele_frequency,
                        approx=approximation_s1$V2)

g1<-ggplot(simvsapprox_s1, aes(x_cords)) +
  geom_line(aes(y = simulated, color = "modified Gamma(4,0.3) simulation"), linetype = "solid") +
  geom_line(aes(y = approx, color = "Model approximation"), linetype = "dashed")+
  ylim(0,1)+theme_bw()+ylab("Allele Frequency") + xlab("X cordinates")+
  scale_color_manual(name = "Line Type",aes(show.legend=FALSE),values = c("modified Gamma(4,0.3) simulation" = "black", "Model approximation" = "blue"))+ 

  annotate("rect", xmin = -250, xmax = 0, ymin = 0, ymax = 1, fill = "red", alpha = 0.5) +xlim(-250,250)+
#  geom_label(aes(x = -150, y = 0.1, label = "Negative Selection"), fill = "red", alpha = 1)+
  
  annotate("rect", xmin = 0, xmax = 250, ymin = 0, ymax = 1, fill = "green", alpha = 0.5) 
 # geom_label(aes(x = 150, y = 0.1, label = "Postive Selection"), fill = "green", alpha = 1)

ggsave(file = "cline1.png",g1, dpi = 600, width = 8, height = 6, units = "in")+ theme(legend.position = "none")





##############################
#Generate plot for selection scheme s2




simulated_s2<-read.csv("data/s2_4_03_simulated_b10.txt",header = TRUE)
simulated_s2<-simulated_s2[1:999,]

approximation_s2<-read.csv("data/s2_approx_4_03_b10.txt",header = FALSE)
approximation_s2<-approximation_s2[1:999,]

simvsapprox_s2<-data.frame(x_cords=simulated_s2$x_cords,simulated=simulated_s2$allele_frequency,
                           approx=approximation_s2$V2)

g2<-ggplot(simvsapprox_s2, aes(x_cords)) +
  geom_line(aes(y = simulated, color = "modified Gamma(4,0.3) simulation"), linetype = "solid") +
  geom_line(aes(y = approx, color = "Model approximation"), linetype = "dashed")+
  ylim(0,1)+theme_bw()+ylab("Allele Frequency") + xlab("X cordinates")+
  scale_color_manual(name = "Line Type", values = c("modified Gamma(4,0.3) simulation" = "black", "Model approximation" = "blue"))+ 
  
  annotate("rect", xmin = -bound, xmax = bound, ymin = 0, ymax = 1, fill = "red", alpha = 0.5) + xlim(-250,250)+
  annotate("rect", xmin = -250, xmax = -bound, ymin = 0, ymax = 1, fill = "green", alpha = 0.5)+
  #geom_label(aes(x = -150, y = 0, label = "Positive selection"), fill = "green", alpha = 1)+
  annotate("rect", xmin = bound, xmax = 250, ymin = 0, ymax = 1, fill = "green", alpha = 0.5)
  #geom_label(aes(x = 150, y = 0, label = "Positive selection"), fill = "green", alpha = 1)+
 # geom_label(aes(x = 0, y = 0.1, label = "Negative selection"), fill = "red", alpha = 1)+ theme(legend.position = "none")



ggsave(file = "cline2.png",g2, dpi = 600, width = 8, height = 6, units = "in")

###########
#Generate plot for selection scheme s3

simulated_s3<-read.csv("data/s3_4_03_simulated_b10.txt",header = TRUE)
simulated_s3<-simulated_s3[1:999,]

approximation_s3<-read.csv("data/s3_approx_4_03_b10.txt",header = FALSE)
approximation_s3<-approximation_s3[1:999,]

simvsapprox_s3<-data.frame(x_cords=simulated_s3$x_cords,simulated=simulated_s3$allele_frequency,
                           approx=approximation_s3$V2)

g3<-ggplot(simvsapprox_s3, aes(x_cords)) +
  geom_line(aes(y = simulated, color = "modified Gamma(4,0.3) simulation"), linetype = "solid") +
  geom_line(aes(y = approx, color = "Model approximation"), linetype = "dashed")+
  ylim(0,1)+theme_bw()+ylab("Allele Frequency") + xlab("X cordinates")+
  scale_color_manual(name = "Line Type", values = c("modified Gamma(4,0.3) simulation" = "black", "Model approximation" = "blue","Positive selection"="green","Negative selection"="red"))+ 
  
  annotate("rect", xmin = -bound, xmax = bound, ymin = 0, ymax = 1, fill = "green", alpha = 0.5) + xlim(-250,250)+
  annotate("rect", xmin = -250, xmax = -bound, ymin = 0, ymax = 1, fill = "red", alpha = 0.5)+
 # geom_label(aes(x = -150, y = 0, label = "Negative selection"), fill = "red", alpha = 1)+
  annotate("rect", xmin = bound, xmax = 250, ymin = 0, ymax = 1, fill = "red", alpha = 0.5)
#  geom_label(aes(x = 150, y = 0, color = "Negative selection"), fill = "red", alpha = 1)+
#  geom_label(aes(x = 0, y = 0.1, color = "Positive selection"), fill = "green", alpha = 1)



ggsave(file = "cline3.png",g3, dpi = 600, width = 8, height = 6, units = "in")

library(ggpubr)
library(cowplot)

combo<-ggarrange(g1+ rremove("ylab") + rremove("xlab"), g2+ rremove("ylab") + rremove("xlab"), g3+ rremove("ylab") + rremove("xlab"), ncol=1, nrow=3, common.legend = TRUE, legend="top")


require(grid)   # for the textGrob() function

combo_final_plot<-annotate_figure(combo, left = textGrob("Allele Frequency", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Position", hjust =0.5, gp = gpar(cex = 1)))
combo_final_plot
save_plot("4_03_s1s2s3_b10.jpg",combo_final_plot)
