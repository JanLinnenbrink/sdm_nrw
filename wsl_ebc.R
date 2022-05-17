
# Load necessary libraries 
require(raster)
require(cluster)

wsl.ebc=function(obs=NULL,ras=NULL,pportional=TRUE,plog=TRUE,nclust=50,
                 sp.specific=TRUE,sp.cor=0.5,keep.bias=TRUE,filter=FALSE,path=NULL,...)
{

  
  # 'nclust' must be at least equal to '2'
  if (nclust<=1 || is.null(nclust)){
    stop("'nclust' must be equal to 2 or more...!")
  }
  
  # 'obs' must be a data.frame of observations with column "sp.id","x" and "y"
  if (!(class(obs)%in%c("data.frame","matrix"))){
    stop("'obs' must be an object of class 'data.frame' or 'matrix'...!")
  }
  
  if (class(obs)%in%"matrix") {
    obs=as.data.frame(obs,stringsAsFactors=FALSE)
    obs$x=as.numeric(obs$x)
    obs$y=as.numeric(obs$y)
  }
  
  if (any(table(obs$sp.id)%in%1)){
    stop("Each species must have more than one observation...!")
  }
  
  # 'ras' must be of 'brick' or 'stack' class or list of 'rasters'
  if (!(class(ras)%in%c("RasterBrick","RasterStack","list"))){
    stop("'ras' must be an object of class 'RasterBrick', 'RasterStack' or 'list'...!")
  } else if (length(ras)%in%1 || nlayers(ras)%in%1){
    stop("'ras' must include more than one 'RasterLayer'...!")
  }
  if (class(ras)%in%"list"){
    ras=stack(ras)
  }
  
  # First info message
  cat("Step 1 --> CLARA algorithm processing...","\n")
  
  # Convert raster in the right CLARA format
  id.toReplace=complete.cases(ras[])
  toClara=ras[][id.toReplace,]
  
  # Run CLARA and assign results to right pixels
  blocks=clara(toClara,nclust,...)
  toNew.ras=ras[[1]]
  toNew.ras[][id.toReplace]=blocks$clustering
  toNew.ras[][!id.toReplace]=NA
  
  # Start inventorying species observations per cluster 
  sp.ref=as.character(unique(obs$sp.id))
  
  # Second info message
  cat("Step 2 --> Species inventory...","\n")
  
  cluster.infos=list()
  cluster.tab=list()
  for (i in 1:length(sp.ref))
  {
    # Extract each species one by one & filter according to grid resolution if TRUE
    sp.target=obs[obs$sp.id%in%sp.ref[i],c("x","y")]
    if (filter){
      
      # Load necessary sub-function 
      wsl.obs.filter=function(o.xy,a.xy=NULL,grid)
      {
        # Check 'o.xy' input
        if(ncol(o.xy)!=2 || !all(colnames(o.xy)%in%c("x","y"))){
          stop("Supplied points should be a data.frame/matrix with two columns named x and y!")
        }
        
        # Check 'ras' input
        if(!(class(grid)%in%c("RasterBrick","RasterStack","RasterLayer"))) {
          stop("env.layer should be of class RasterBrick, RasterStack or RasterLayer!")
        }
        
        # Apply simple filtering
        if (!is.null(a.xy)) {
          
          # Check 'a.xy' input
          
          if(ncol(a.xy)!=2 || !all(colnames(a.xy)%in%c("x","y"))){
            stop("Supplied points should be a data.frame/matrix with two columns named x and y!")
          }
          
          # Position presences and absences
          posP=cellFromXY(grid,o.xy)
          posA=cellFromXY(grid,a.xy)
          
          # Remove absences where we find presences
          posA=posA[!(posA %in% posP)]
          
          # Extract new presences/absences and regroup
          new.pxy=coordinates(grid)[unique(posP),]
          new.axy=coordinates(grid)[unique(posA),]
          new.oxy=list(new.pxy,new.axy)
          names(new.oxy)=c("Presences","Absences")
          
        } else {
          # Apply simple filtering
          posCELL=cellFromXY(grid,o.xy)
          new.oxy=coordinates(grid)[unique(posCELL),]
        }
        
        if (class(new.oxy)[1]%in%"numeric"){
          new.oxy=matrix(new.oxy,ncol=2)
          colnames(new.oxy)=c("x","y")
        }
        
        return(new.oxy)
      }
      
      sp.target=wsl.obs.filter(sp.target[,c("x","y")],grid=toNew.ras)
    }
    
    # Extract cluster values with warnings if XY outside raster extent or assign to NA values
    clustV=extract(toNew.ras,sp.target)
    if(any(is.na(clustV))) {
      warning(paste("XY coordinates of",sp.ref[i],
                    "outside 'ras' boundaries, or have extracted NAs environmental values...",sep=" "))
    }
    summaryV=table(clustV)
    
    # Fill a vector of length 'nclust'
    sp.vec=rep(0,nclust)
    sp.vec[1:nclust%in%as.numeric(names(summaryV))]=summaryV
    
    # Store infos
    cluster.infos[[i]]=cbind(sp.target,clustV)
    cluster.tab[[i]]=c(sp.ref[i],sp.vec)
  }
  
  # Combine and apply a sum to all columns
  cluster.all=as.data.frame(do.call("rbind",cluster.tab))
  cluster.all[,2:(nclust+1)]=sapply(cluster.all[,2:(nclust+1)],
                                    function(x) as.numeric(as.character(x)))
  cluster.sum=apply(cluster.all[,-1],2,sum)
  
  # Which cluster is max?
  plateau=max(cluster.sum)
  
  # Calculate area values based on n pixels per cluster (formated)
  pixels.clust=table(toNew.ras[])
  area.clust=pixels.clust/sum(pixels.clust)*100
  
  # Get reduction coef for each cluster specific to "plateau"
  reducoef=cluster.sum/plateau
  
  # Apply it to the species table and round up to the integer
  down.l=lapply(1:length(reducoef),function(x) cluster.all[,1+x]/reducoef[x])
  cluster.down=do.call("cbind",down.l)
  cluster.down=ceiling(cluster.down)
  
  # Weight observations by cluster areas
  if (pportional){
    cat ("...Environmental proportional stratification = TRUE...","\n")
    
    if (plog){
      cat ("...log...","\n")
      area.clust=log(area.clust+1)
    }
    
    down.l2=lapply(1:length(reducoef),function(x) cluster.down[,x]*area.clust[x])
    cluster.down2=do.call("cbind",down.l2)
    cluster.down=ceiling(cluster.down2)
    
  } else {
    cat ("...Environmental proportional stratification = FALSE...","\n")
  }
  
  # Replace NAs by 0 in case absolutely no informations fall in one cluster
  cluster.down[is.na(cluster.down)]=0
  
  # Store
  cl.out=data.frame(x0=cluster.all[,1],cluster.down)
  cl.out[,1]=as.character(cl.out[,1])
  
  # Do we apply EBC only for target species?
  if (!(sp.specific)){
    cat("Step 3 --> EBC set to non species-specific...","\n")
  } else {
    cat("Step 3 --> EBC set to species-specific...","\n")
    
    # Tchao species whose environmental bias is not correlated with the general one
    spcl.temp=
      lapply(1:nrow(cluster.all),function(x) {
        sp.sumo=as.numeric(cluster.all[x,-1])
        cor.ebc=cor(sp.sumo,cluster.sum)
        if (cor.ebc>=sp.cor | is.na(cor.ebc)){
          return(cl.out[x,])
        }
      })
    # Format cluster.infos & bind results
    ebc.null=sapply(spcl.temp,is.null)
    cluster.infos=cluster.infos[!ebc.null]
    cl.out=do.call("rbind",spcl.temp)
  }
  
  # Should the initial environmental bias be preserved?
  if (!keep.bias){
    cat("Step 4 --> Initial environmental bias per species is not preserved...","\n")
  } else {
    cat("Step 4 --> Initial environmental bias per species is preserved...","\n")
    
    # Keeping initial biased environment
    spcl.temp2=
      lapply(1:nrow(cl.out),function(x) {
        spcl.out=cl.out[x,]
        o.clust=cluster.all[cluster.all[,1]%in%spcl.out[,1],]
        maxo=which.max(o.clust[,-1])[1]
        spcl.out[,maxo+1]=max(spcl.out[,-1],na.rm=TRUE)
        return(spcl.out)
      })
    # Format cluster.infos & bind results
    cl.out=do.call("rbind",spcl.temp2)
  }
  
  # Fourth infos message
  cat("Step 5 --> Environmental Bias Correction (EBC) starting...","\n")
  
  # Write new observation files per species in choosen 'path'
  for (i in 1:nrow(cl.out))
  {
    # Extract number of obs. per cluster to sample
    sp.infos=cl.out[i,]
    sp.save=as.character(sp.infos[1])
    
    # Extract original observations from species
    cl.i=cluster.infos[[i]]
    
    # Fourth infos message
    cat("Processing ",sp.save,"...","\n")
    
    # Extract obs. according to each cluster
    Cselect=list()
    for (j in 1:nclust) {
      
      # Extract observations in clusters
      tg=cl.i[cl.i[,"clustV"]%in%j,c("x","y"),drop=FALSE]
      
      # Sample with replacements
      n.samp=as.numeric(sp.infos[-1][j])
      do.samp=tg[sample(1:nrow(tg),n.samp,replace=TRUE),c("x","y")]
      
      # Store with cluster ID depending on the output
      if (nrow(do.samp)%in%0){
        next
      } else if (class(do.samp)[1] %in% "numeric") {
        do.samp=matrix(do.samp,ncol=2)
        colnames(do.samp)=c("x", "y")
      }
      
      Cselect[[j]]=data.frame(cluster.id=paste0("clust.",j),do.samp,stringsAsFactors=FALSE)
    }
    
    # Rbind & save
    new.obs=do.call("rbind",Cselect)
    out.sp=paste0(path,"/",Sys.Date(),"_obs_corrected_",sp.save,".txt")
    write.table(new.obs,out.sp)
  }
}
