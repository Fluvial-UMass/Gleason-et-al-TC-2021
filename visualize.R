
library(tidyr)
library(dplyr)
library(rgdal)
library(sf)
library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(cowplot)
library(stringr)


setwd('this directory')


plot_network=1
plot_bar_chart =1
plot_hydrographs=1
plot_ns=1

n_generate=readRDS('mannings_n.rds')
hydrograph_df=readRDS('all_hydrograph_df_validation.rds')
results_table=readRDS('results_table.rds')
unrouted_runoff=readRDS('input_runoff.rds') %>%
  gather(model,input_runoff)%>%
  filter(model != 'Time')

HNH_list=c('Hillslope','Hillslope','Non Hillslope','Non Hillslope','Hillslope','Hillslope','Non Hillslope','Non Hillslope',
           'Hillslope','Hillslope','Non Hillslope','Non Hillslope','Hillslope','Hillslope','Non Hillslope','Non Hillslope')
CF_list=c('Fine','Coarse','Fine','Coarse','Fine','Coarse','Fine','Coarse','Fine','Coarse','Fine','Coarse','Fine','Coarse','Fine','Coarse')


if (plot_hydrographs ==1){
  
  
  #use HIRHAM as an example
  hydro_HIR=hydrograph_df %>%
    select(Time,Q_true,HIR,HIR10kH,HIR1kH, HIR1kNH,HIR10kNH) %>%
    #select(Time,Q_true,HIR,HIR10kH,HIR1kH) %>%
    gather('source','value',-Time)
  
  
  plot_HIR_legend = ggplot(hydro_HIR)+
    geom_line(aes(x= Time, y=value,col=source ),lwd=0.5) +
    scale_color_manual(values=c('brown','red','magenta','blue','cyan','black'),
                       labels=c('Instantaneous','Fine Hillslope','Fine Non Hillslope','Coarse Hillslope','Coarse Non Hillslope', 'ADCP'))+
    #scale_color_manual(values=c('brown','cyan','magenta','black'),labels=c('Instantaneous','Fine','Coarse ', 'ADCP'))+
    # scale_size_manual(values=c(1,2,2),guide=FALSE)+
    #the above line is brutal. Mapping to size values requires that the sizes match the differing values in the tidy df. So,
    #the fact that i've set the values to 2 and 4 are irrelevant, the fact that there are two differnt values is important.
    #also, note the need for an 'as.factor' argument
    xlim(c(0,72))+
    # ylim(c(0,70))+
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab('Time (hr)') +
    theme_bw() +
    ggtitle('HIRHAM')+
    # theme(legend.text=element_text(size=15),
    theme(legend.text=element_text(size=8),
          legend.title=element_blank(),
          legend.position ='right',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  plot_HIR = ggplot(hydro_HIR)+
    geom_line(aes(x= Time, y=value,col=source ),lwd=0.5) +
    scale_color_manual(values=c('brown','red','magenta','blue','cyan','black'),
                       labels=c('Instantaneous','Fine Hillslope','Fine Non Hillslope','Coarse Hillslope','Coarse Non Hillslope', 'ADCP'))+
    #scale_color_manual(values=c('brown','cyan','magenta','black'),labels=c('Instantaneous','Fine','Coarse ', 'ADCP'))+
    # scale_size_manual(values=c(1,2,2),guide=FALSE)+
    #the above line is brutal. Mapping to size values requires that the sizes match the differing values in the tidy df. So,
    #the fact that i've set the values to 2 and 4 are irrelevant, the fact that there are two differnt values is important.
    #also, note the need for an 'as.factor' argument
    xlim(c(0,72))+
    # ylim(c(0,70))+
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab('Time (hr)') +
    theme_bw() +
    ggtitle('HIRHAM')+
    # theme(legend.text=element_text(size=15),
    theme(legend.text=element_text(size=6),
          legend.title=element_blank(),
          legend.position ='none',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  
  # 
  # plot(plot_HIR)
  
  #use HIRHAM as an example
  hydro_MER=hydrograph_df %>%
    select(Time,Q_true,MER,MER10kH,MER1kH, MER1kNH,MER10kNH) %>%
    gather('source','value',-Time)
  
  
  plot_MER = ggplot(hydro_MER)+
    geom_line(aes(x= Time, y=value,col=source ),lwd=0.5) +
    scale_color_manual(values=c('brown','red','magenta','blue','cyan','black'),
                       labels=c('Instantaneous','Fine Hillslope','Fine Non Hillslope','Coarse Hillslope','Coarse Non Hillslope', 'ADCP'))+
    # scale_size_manual(values=c(1,2,2),guide=FALSE)+
    #the above line is brutal. Mapping to size values requires that the sizes match the differing values in the tidy df. So,
    #the fact that i've set the values to 2 and 4 are irrelevant, the fact that there are two differnt values is important.
    #also, note the need for an 'as.factor' argument
    xlim(c(0,72))+
    # ylim(c(0,70))+
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab('Time (hr)') +
    theme_bw() +
    ggtitle('MERRA 2')+
    theme(legend.text=element_text(size=6),
          legend.title=element_blank(),
          legend.position ='none',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  
  # 
  # plot(plot_MER)
  
  #use HIRHAM as an example
  hydro_Racmo=hydrograph_df %>%
    select(Time,Q_true,Racmo,Racmo10kH,Racmo1kH, Racmo1kNH,Racmo10kNH) %>%
    gather('source','value',-Time)
  
  
  plot_Racmo = ggplot(hydro_Racmo)+
    geom_line(aes(x= Time, y=value,col=source ),lwd=0.5) +
    scale_color_manual(values=c('black','brown','red','magenta','blue','cyan'),
                       labels=c('ADCP', 'Instantaneous','Fine Hillslope','Fine Non Hillslope','Coarse Hillslope','Coarse Non Hillslope' ))+
    # scale_size_manual(values=c(1,2,2),guide=FALSE)+
    #the above line is brutal. Mapping to size values requires that the sizes match the differing values in the tidy df. So,
    #the fact that i've set the values to 2 and 4 are irrelevant, the fact that there are two differnt values is important.
    #also, note the need for an 'as.factor' argument
    xlim(c(0,72))+
    # ylim(c(0,70))+
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab('Time (hr)') +
    theme_bw() +
    ggtitle('RACMO 2')+
    theme(legend.text=element_text(size=6),
          legend.title=element_blank(),
          legend.position ='none',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  # 
  # 
  # plot(plot_Racmo)
  
  #use HIRHAM as an example
  hydro_MAR=hydrograph_df %>%
    select(Time,Q_true,MAR,MAR10kH,MAR1kH, MAR1kNH,MAR10kNH) %>%
    gather('source','value',-Time)
  
  
  plot_MAR = ggplot(hydro_MAR)+
    geom_line(aes(x= Time, y=value,col=source ),lwd=0.5) +
    scale_color_manual(values=c('brown','red','magenta','blue','cyan','black'),
                       labels=c('Instantaneous','Fine Hillslope','Fine Non Hillslope','Coarse Hillslope','Coarse Non Hillslope', 'ADCP'))+
    # scale_size_manual(values=c(1,2,2),guide=FALSE)+
    #the above line is brutal. Mapping to size values requires that the sizes match the differing values in the tidy df. So,
    #the fact that i've set the values to 2 and 4 are irrelevant, the fact that there are two differnt values is important.
    #also, note the need for an 'as.factor' argument
    xlim(c(0,72))+
    # ylim(c(0,70))+
    ylab(expression(paste("Q (",m^3, "/s)", sep="")))+
    xlab('Time (hr)') +
    theme_bw() +
    ggtitle("MAR")+
    theme(legend.text=element_text(size=6),
          legend.title=element_blank(),
          legend.position ='none',
          axis.text= element_text(size=8),
          axis.title= element_text(size=8),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  
  # plot(plot_MAR)
  
  
  extra_legend=cowplot::get_legend(plot_HIR_legend)
  
  figure5= plot_grid(plot_grid(plot_HIR,plot_MER,extra_legend, nrow=1,rel_widths = c(1,1,0.4)),plot_grid(plot_MAR,plot_Racmo,NULL, nrow=1,rel_widths = c(1,1,0.4)),nrow=2)
  #figure5=plot_grid(plot_HIR,plot_MER,plot_MAR,plot_Racmo,extra_legend,nrow=3,ncol=2, rel_heights = c(1,1,0.4))
  plot(figure5)
  
  #hydrographs
  ggsave(filename="hydrographs.jpg", 
         plot=figure5,
         width=8, 
         height=5, 
         dpi= 500)
  
  
}
if (plot_bar_chart ==1){
  

  ADCP_cumulative_runoff=max(cumsum(hydrograph_df$Q_true))
  
  cumulative_runoff_plot=results_table %>%
    #overwrite total time period runoff with 72 hour runoff
    select(-input_runoff)%>%
    left_join(unrouted_runoff,by='model')%>%
    mutate(modified_runoff=input_runoff*Rcoef)%>%
    #add ADCP runoff
    mutate(ADCP=ADCP_cumulative_runoff)%>%
    
    #fix nomenclature for publication quality plot
    mutate(Network=paste(CF_list,HNH_list)) %>%
    select(model,input_runoff,modified_runoff,Network,NSE,ADCP) %>%
    filter(Network=='Fine Hillslope' | Network =='Coarse Hillslope') %>%
    gather(source,value,-model,-Network,-NSE) %>%
    mutate(source= ifelse(source=='modified_runoff',Network,source)) %>%
    mutate(NSE= ifelse(source=='input_runoff',"",NSE)) %>%
    select(-Network,-NSE) %>%
    distinct() %>%
    #code wants to plot in ABC order, so order according to a scheme. Inelegant but effective
    mutate(source = ifelse(source == 'input_runoff','A',source)) %>%
    mutate(source = ifelse(source == 'Fine Hillslope','B',source)) %>%
    mutate(source = ifelse(source == 'Coarse Hillslope','C',source)) %>%
    mutate(source = ifelse(source == 'ADCP','D',source)) %>%
    mutate(model = ifelse(model == 'Racmo','RACMO2',model))%>%
    mutate(model = ifelse(model == 'MER','MERRA2',model)) %>%
    mutate(model = ifelse(model == 'HIR','HIRHAM',model)) 
  
  # mutate(model= rep(rep(c('HIRHAM','MAR','MERRA',"RACMO"),each=4),times=2 )  )  %>%
  #filter(source=='input_runoff' | source == '10k H' | source == '1k H')
  
  figure4=ggplot(cumulative_runoff_plot, aes(x=model,y=value, fill=source)) +
    geom_bar(stat="identity", position=position_dodge2())+
    # scale_fill_discrete(labels=c('Fine Network Hillslope','Fine Network non-Hillslope',
    #                              'Coarse Network','Coarse Network non-Hillslope','Un-routed'))+
    scale_fill_discrete(labels=c('Instantaneous Routing', 'Fine Network','Coarse Network',"ADCP"))+
    scale_y_continuous(labels=scales::comma)+
    #geom_text(aes(label=NSE), position=position_dodge(0.9), vjust=1.5, size=3.5)+
    ylab(expression(paste("Cumulative runoff ( ",m^3, ")", sep="")))+
    # xlab('Time (hr)') +
    theme_bw() +
    theme(legend.text=element_text(size=6),
          legend.title=element_blank(),
          legend.position = c(0.5,.95),
          axis.text= element_text(size=8),
          axis.text.x= element_text(angle=45,hjust=1),
          axis.title.y= element_text(size=8),
          axis.title.x= element_blank(),
          legend.justification = 'top',
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  plot(figure4)
  
  
  #bar chart
  ggsave(filename="bar_chart.jpg", 
         plot=figure4,
         width=5, 
         height=5, 
         dpi= 500)
  
  
  
  
}
if (plot_network ==1){
  
  plot_network_coarse=read_sf('HRR_rivers','FlowAcc_WQ7_SETSM_WV01_20150718_burn_stream10000_spatial_join_RS_rivers')
  
  plot1=ggplot(plot_network_coarse) +
    geom_sf(col='blue',show.legend = FALSE,lwd=0.25) +
    # geom_line(aes(x=-170000,y=-2508000,size=HIR1kH),col='blue')+
    # annotate('text',x=-170000,y=-2508000, label= format(time_base+ i*3600,"%Y-%m-%d %H:%M"), size=10) +
    # scale_size_continuous( name=expression(paste("Discharge (",m^3, "/s)", sep="")),
    #                        breaks = plotbreaks_flow,
    #                        limits=range(plotbreaks_flow)) +
    ylab('')+
    xlab('') +
    theme_bw() +
    theme(legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          axis.text= element_text(size=8))
  
  
  
  plot_network_fine=read_sf('HRR_rivers','FlowAcc_WQ7_SETSM_WV01_20150718_burn_stream1000_spatial_join_RS_rivers')
  
  plot2=ggplot(plot_network_fine) +
    geom_sf(col='blue',show.legend = FALSE,lwd=0.01) +
    # geom_line(aes(x=-170000,y=-2508000,size=HIR1kH),col='blue')+
    # annotate('text',x=-170000,y=-2508000, label= format(time_base+ i*3600,"%Y-%m-%d %H:%M"), size=10) +
    # scale_size_continuous( name=expression(paste("Discharge (",m^3, "/s)", sep="")),
    #                        breaks = plotbreaks_flow,
    #                        limits=range(plotbreaks_flow)) +
    ylab('')+
    xlab('') +
    theme_bw() +
    theme(legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          axis.text= element_text(size=8))
  
  #read in a greenland shapefile
  greenland=read_sf('HRR_Rivers','GRL_adm0')
  
  mypoint <- data.frame(long = -48.9, lat = 67)
  
   g2 <-     ggplot(greenland) +
      geom_sf(data = greenland, col= 'black', show.legend = FALSE,lwd=0.5)+
                  
      geom_point(data = mypoint, aes(x = long, y = lat),
                 color = "red", size = 2) +
     xlim(c(-70,-15))+
     ylim(c(59,84))+

      theme(panel.background = element_rect(fill = NULL)) +
     theme_bw()+
     theme(axis.text=element_blank(),
           axis.title=element_blank())
   
  
  plot3 <- ggdraw() +
    draw_plot(plot1) +
    draw_plot(g2, x = 0.25, y =0.20, width =0.25,height=0.4 )
  
  figure3= plot_grid(plot3,plot2,  nrow=1)
  plot(figure3)
  
  #networks
  ggsave(filename="networks.jpg", 
         plot=figure3,
         width=6, 
         height=5, 
         dpi= 500)
  
  
  
}
if (plot_ns==1){
  
  n_df=data.frame(n_generate) %>%
    rename(Model=X1, n=X2) %>%
    mutate(bin_number= paste('bin',rep(1:9,length=nrow(n_generate)))) %>%
    mutate(n=as.numeric(levels(n))[n]) %>%
    mutate(hillslope=str_match(Model,"_(.*)_")[,2]) %>%
    mutate(density = substring(Model, regexpr("k", Model) -2))%>%
    mutate(density = ifelse(density =='10k','Fine','Coarse')) %>%
    select(-Model)%>%
    gather(stat, value, -bin_number,-n) %>%
    select(-stat)

  n_HNH_plot= n_df %>%
    filter(value== 'H' | value =="NH")%>%
    mutate(value= ifelse(value=='H','Hillslope','Non-Hillslope'))
  
  n_CF_plot= n_df %>%
    filter(value== 'Coarse' | value =="Fine")
  
  bin_labels = c('0.010','0.025','0.063','0.200','0.500','1.260','3.160','10', '>10')

  plot_nbins_HNH = ggplot(n_HNH_plot)+
    geom_boxplot(aes(x=bin_number, y= n, fill=value)) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10)) +
    scale_x_discrete(labels=bin_labels)+

    ylab("Manning's n")+
    xlab(expression(paste("Bin maxmimum upstream area (",km^2,")", sep="")))+
    theme_bw() +
    theme(legend.text=element_text(size=8),
          legend.title=element_blank(),
          legend.position =c(0.5,0.8),
          axis.text= element_text(size=10),
          axis.text.x =element_text(angle=45),
      #    axis.text.x=element_blank(),
          axis.title= element_text(size=12),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  plot_nbins_CF = ggplot(n_CF_plot)+
    geom_boxplot(aes(x=bin_number, y= n, fill=value)) +
    scale_fill_manual(values=c('#f7fcb9','#31a354'))+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10)) +
    scale_x_discrete(labels=bin_labels)+
    
    ylab("Manning's n")+
    xlab(expression(paste("Bin maxmimum upstream area (",km^2,")", sep="")))+
    theme_bw() +
    theme(legend.text=element_text(size=8),
          legend.title=element_blank(),
          legend.position =c(0.3,0.8),
          axis.text= element_text(size=10),
          axis.text.x =element_text(angle=45),
          #    axis.text.x=element_blank(),
          axis.title= element_text(size=12),
          plot.title = element_text(color="black", size=8, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  figure6=plot_grid(plot_nbins_HNH,plot_nbins_CF,nrow=1)

  plot(figure6)
  
  #manning's
  ggsave(filename="mannings.jpg", 
         plot=figure6,
         width=8, 
         height=5, 
         dpi= 500)
  
  
  
  
}





