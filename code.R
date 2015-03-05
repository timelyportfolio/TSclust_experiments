library(TSclust)
library(quantmod)
library(dplyr)
library(pipeR)
library(tidyr)
library(epiwidgets)

sp5 <- getSymbols("^GSPC",auto.assign=F,from="1900-01-01")[,4]

sp5 %>>%
  # dplyr doesn't like xts, so make a data.frame
  (
    data.frame(
      date = index(.)
      ,price = .[,1,drop=T]
    )
  ) %>>%
  # add a column for Year
  mutate( year = as.numeric(format(date,"%Y"))) %>>%
  # group by our new Year column
  group_by( year ) %>>%
  # within each year, find what day in the year so we can join
  mutate( pos = rank(date) ) %>>%
  mutate( roc = price/lag(price,k=1) - 1 ) %>>%
  # can remove date
  select( -c(date,price) ) %>>%
  as.data.frame %>>%
  # years as columns as pos as row
  spread( year, roc ) %>>%
  # remove last year since assume not complete
  ( .[,-ncol(.)] ) %>>%
  # remove pos since index will be same
  select( -pos ) %>>%
  # fill nas with previous value
  na.fill( 0 ) %>>%
  t %>>%
(~sp_wide) %>>%
  # use TSclust diss; notes lots of METHOD options
  diss( METHOD="ACF" ) %>>%
  hclust %>>%
(~hc) %>>%
  ape::as.phylo() %>>% 
  treewidget #%>>%
  #htmlwidgets::as.iframe(file="index.html",selfcontained=F,libdir = "./lib")

library(lattice)
library(ggplot2)
# get wide to long the hard way
#  could have easily changed to above pipe to save long
#  as an intermediate step
#  but this makes for a fun lapply
#  and also we can add in our cluster here
sp_wide %>>%
  (
    lapply(
      rownames(.)
      ,function(yr){
        data.frame(
          year = as.Date(paste0(yr,"-01-01"),"%Y-%m-%d")
          ,cluster = cutree(hc,10)[yr]
          ,pos = 1:length(.[yr,])
          ,roc = .[yr,]
        )
      }
    )
  ) %>>%
  (do.call(rbind,.)) %>>%
(~sp_long)

sp_long %>>%
  (densityplot(~roc|cluster,groups=year, col="gray50", data = .))

sp_long %>>%
  ggplot( aes( x = factor(cluster), y = roc )) %>>%
    +geom_violin() + theme_bw()

sp_long %>>%
  ggplot( aes( x = roc, group = year, color = factor(cluster) ) ) %>>%
  + geom_density() %>>%
  + facet_wrap(  ~ cluster, ncol = 1 ) %>>%
  + xlim(-0.05,0.05) %>>%
  + labs(title='Density of S&P 500 Years Clustered by TSclust') %>>%
  + theme_bw() %>>%
  # thanks to my friend Zev Ross for his cheatsheet
  + theme( plot.title = element_text(size=15, face="bold", hjust=0) ) %>>%
  + theme( legend.position="none" )  %>>%
  + scale_color_brewer( palette="Paired" )
  

# explore autocorrelations
sp5 %>>%
  # dplyr doesn't like xts, so make a data.frame
  (
    data.frame(
      date = index(.)
      ,price = .[,1,drop=T]
    )
  ) %>>%
  # add a column for Year
  mutate( year = as.numeric(format(date,"%Y"))) %>>%
  # group by our new Year column
  group_by( year ) %>>%
  # within each year, find what day in the year so we can join
  mutate( pos = rank(date) ) %>>%
  mutate( roc = price/lag(price,k=1) - 1 ) %>>%
  # can remove date
  select( -c(date,price) ) %>>%
  as.data.frame %>>%
  # years as columns as pos as row
  spread( year, roc ) %>>%
  # remove last year since assume not complete
  ( .[,-ncol(.)] ) %>>% t -> sP

sp_long %>>%
  group_by( cluster, year ) %>>%
  do(
    . %>>%
    (
      clustd ~ 
      acf(clustd$roc,plot=F) %>>%
        (a ~
          data.frame(
            cluster = clustd[1,2]
            ,year = clustd[1,1]
            ,lag = a$lag[-1]
            ,acf = a$acf[-1]
          )
        )
    )
  ) %>>%
  as.data.frame %>>%
  ggplot( aes( x = factor(cluster), y = acf, color = factor(cluster) ) ) %>>%
    + geom_point() %>>%
    + facet_wrap( ~lag, ncol = 4 ) %>>%
    + labs(title='ACF of S&P 500 Years Clustered by TSclust') %>>%
    + theme_bw() %>>%
    # thanks to my friend Zev Ross for his cheatsheet
    + theme(
        plot.title = element_text(size=15, face="bold", hjust=0)
        ,legend.title=element_blank()
    ) %>>%
    + theme(legend.position="none")  %>>%
    + scale_color_brewer(palette="Paired")
