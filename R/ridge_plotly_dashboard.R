


plotlyridges <- function(data,
                         numeric_cols,
                      vardens,
                      varcat,
                      linecolor='darkblue',
                      fillcolor='steelblue',
                      fillopacity=0.6,
                      linewidth=0.5,
                      scale=0.9,
                      logspaced=FALSE,cut.from=0, cut.to=3,n=512,bw=NULL,bw.separate=FALSE,height.norm='integral',round.digits=2,x.min=0
                      ,height=NULL
                      ,width=NULL
){

  data=subset(data,!is.na(data[,vardens]))

  r=range(data[,vardens])

  if(is.null(bw)){
    if(!bw.separate){
      if(logspaced){
        x=log(data[,vardens])
      }else{
        x=data[,vardens]
      }
      if(length(x)>1){
        bw=density(x)$bw
      }else{
        bw=1
      }
    }
  }

  df=aggregate(data[,vardens],by=list(varcat=data[,varcat]),FUN=function(x){

    if(length(x)==0) return(NULL)
    if(bw.separate){
      if(length(x)==1){
        bw=1
      }else{
        bw='nrd0'
      }
    }
    if(logspaced){
      x=log(x)
    }
    from=min(x,na.rm=TRUE)-cut.from*bw
    to=max(x,na.rm=TRUE)+cut.to*bw
    d=density(x,bw=bw,n=n,from=from,to=to)[1:2]
    #Normalize height
    if(height.norm=='1'){
      d$y=d$y/max(d$y)
    }
    if(height.norm=='integral'){
      d$y=d$y/(sum(d$y)*(d$x[2]-d$x[1]))
    }

    if(logspaced){
      d$x=exp(d$x)
      d$y=d$y/d$x
    }


    return(d)
  })

  text=aggregate(data[,vardens],by=list(varcat=data[,varcat]),FUN=function(x){

    x=x[!is.na(x)]
    q=quantile(x)
    text=paste0('Observations: ',prettyNum(length(x),big.mark=','),'<br>Median: ',round(q[3],round.digits),'<br>Range: [',round(q[1],round.digits),', ',round(q[5],round.digits),']','<br>Interquartile Range: [',round(q[2],round.digits),', ',round(q[4],round.digits),']')
    return(text)
  })$x




  catnames=df[[1]]
  x=df[[2]][1:(length(df[[1]]))]
  y=df[[2]][-(1:(length(df[[1]])))]



  ymax=max(unlist(y),na.rm=TRUE)
  y=lapply(y,function(yy){
    yy=scale*yy/ymax
    return(yy)
  })
  p=plotly::plot_ly(type='scatter',mode='lines',height=height,width=width)

  fillcolor=as.vector(col2rgb(fillcolor))/255
  fillcolor=rgb(fillcolor[1],fillcolor[2],fillcolor[3],alpha=fillopacity)


  if(is.null(x.min)){
    xaxis=list(range=r)
  }else{
    xaxis=list(range=c(x.min,r[2]))
  }

  for(i in rev(1:length(catnames))){

    p=plotly::add_trace(p,x=x[[i]],y=i, line=list(color=linecolor,width=linewidth),showlegend=FALSE,hoverinfo='none')
    p=plotly::add_trace(p,
                        x=x[[i]],y=y[[i]]+i,fill='tonexty', fillcolor=fillcolor, line=list(color=linecolor,width=linewidth),showlegend=FALSE, name=catnames[i],hoverinfo='text',text=text[i]
    )

    p=plotly::layout(p,
                     yaxis=list(tickmode='array',tickvals=(1:length(catnames)),ticktext=catnames,showline=TRUE)
                     ,xaxis=xaxis
    )

  }
  p
  return(p)
}
