path='F:/allen_cell_type/filtdata/'
setwd(path)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(ggpubr)
windowsFonts(myFont = windowsFont("Times New Roman"))
feature<-read.csv('features_v2.csv')

fi<-qplot(fi_fit_slope,data=feature,geom='density',
         colour=transgenic_line,
         size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
        )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

Rin<-qplot(input_resistance,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

rheobase<-qplot(rheobase_i,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

sag<-qplot(sag,data=feature,geom='density',
                colour=transgenic_line,
                size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

tau<-qplot(tau,data=feature,geom='density',
                colour=transgenic_line,
                size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

baseline<-qplot(v_baseline,data=feature,geom='density',
                colour=transgenic_line,
                size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

ggarrange(fi, Rin, baseline,rheobase,sag,tau, labels = c("A", "B", "C","D","E","F"), ncol = 3, nrow = 3,common.legend = TRUE, legend = "top",align = "v")

amplitude<-qplot(Amplitude,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

threshold<-qplot(Threshold,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

AHP_amp<-qplot(AHP_Amplitude,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

AHP_latency<-qplot(AHP_Latency,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  xlim(0,10)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  #scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

AP_width<-qplot(AP_Width,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

half_width<-qplot(Half_Width,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

rise_time<-qplot(Rise_Time,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  xlim(0.1,1)+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  #scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

rise_rate<-qplot(Rise_Rate,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

fall_time<-qplot(Fall_Time,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

fall_rate<-qplot(Fall_Rate,data=feature,geom='density',
          colour=transgenic_line,
          size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

up_down_stroke_ratio<-qplot(up_down_stroke_ratio,data=feature,geom='density',
                            colour=transgenic_line,
                            size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

ggarrange(threshold, amplitude, AHP_amp,AHP_latency,AP_width,half_width,rise_time,rise_rate,fall_time,fall_rate, labels = c("A", "B", "C","D","E","F","G","H","I","J"), ncol = 3, nrow = 4,common.legend = TRUE, legend = "top",align = "v")

last_ISI_end<-qplot(last_ISI_end,data=feature,geom='density',
                 colour=transgenic_line,
                 size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

adaption_index<-qplot(adaption_index,data=feature,geom='density',
                    colour=transgenic_line,
                    size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

adaption<-qplot(adaption,data=feature,geom='density',
                      colour=transgenic_line,
                      size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

ISI_cv<-qplot(ISI_cv,data=feature,geom='density',
                      colour=transgenic_line,
                      size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

first_two_ISI_ratio<-qplot(first_two_ISI_ratio,data=feature,geom='density',
                      colour=transgenic_line,
                      size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

X1st_latency<-qplot(X1st_latency,data=feature,geom='density',
                           colour=transgenic_line,
                           size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_amp<-qplot(cv_amp,data=feature,geom='density',
                    colour=transgenic_line,
                    size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_threshold<-qplot(cv_threshold,data=feature,geom='density',
              colour=transgenic_line,
              size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_AP_width<-qplot(cv_AP_width,data=feature,geom='density',
                    colour=transgenic_line,
                    size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_half_width<-qplot(cv_half_width,data=feature,geom='density',
                   colour=transgenic_line,
                   size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_AHP_latency<-qplot(cv_AHP_latency,data=feature,geom='density',
                     colour=transgenic_line,
                     size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_AHP_amp<-qplot(cv_AHP_amp,data=feature,geom='density',
                      colour=transgenic_line,
                      size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_AP_rise_time<-qplot(cv_AP_rise_time,data=feature,geom='density',
                  colour=transgenic_line,
                  size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

cv_AP_fall_time<-qplot(cv_AP_fall_time,data=feature,geom='density',
                       colour=transgenic_line,
                       size=I(0.5))+
  ylab('Density')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = c(0.6,0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=brewer.pal(8,'Paired'))+
  scale_colour_manual(values=brewer.pal(8,'Paired'))

pdf("Sweep_plot.pdf",width = 27,height = 30)
ggarrange(last_ISI_end,adaption_index,adaption,ISI_cv,first_two_ISI_ratio,
          X1st_latency,cv_amp,cv_threshold,cv_AP_width,cv_half_width,
          cv_AHP_latency,cv_AHP_amp,cv_AP_rise_time,cv_AP_fall_time,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N"), 
          ncol = 3, nrow = 5,common.legend = TRUE, legend = "top",align = "v")
dev.off()
