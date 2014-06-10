#You will need to obtain an api-token with appropriate permissions from one or more sources. See:
#http://developers.facebook.com/docs/reference/api/
#https://developers.facebook.com/tools/explorer
library(RCurl)
library(RJSONIO)
library(network)

#This is the function that will be of primary interest to network researchers
# Collects an edgelist (not an sna.edgelistm, but literally a list of edges) of the intersection between i's and j's edges
# The token can be obtained using the facebook graph api browser tool
get.facebook.edgelist<-function(myid,mytoken,attribs=NULL,ret.friends=FALSE,verbose=TRUE){
   friends.call<-paste("https://graph.facebook.com/", myid, "/friends?format=json", "&access_token=", mytoken, sep = "")
   friends.json<-getURL(friends.call)
   friends.dat<-lapply(fromJSON(friends.json)$data, function(x) x[[2]])
   mutuals.dat<-vector("list",length=length(friends.dat))
   if(!is.null(attribs)){my.attribs<-paste("?fields=",paste(attribs,collapse=","),sep="")}
   for(i in 1:length(friends.dat)){
      if(verbose){print(paste(i,friends.dat[[i]]))}
      uid<-friends.dat[[i]]
      my.call<-paste("https://graph.facebook.com/", myid, "/mutualfriends/", uid, "?format=json", "&access_token=", mytoken, sep = "")
      mut.json<-getURL(my.call)
      mutuals.dat[[i]]<-unlist(lapply(fromJSON(mut.json)$data, function(x) x[[2]]))
      if(!is.null(mutuals.dat[[i]])){
         attr(mutuals.dat[[i]],"alter.names")<-unlist(lapply(fromJSON(mut.json)$data, function(x) x[[1]]))
      }
      if(is.null(mutuals.dat[[i]])){mutuals.dat[[i]]<-NA}
      if(!is.null(attribs)){
         uid.call<-paste("https://graph.facebook.com/", uid, my.attribs, "&format=json", "&access_token=", mytoken, sep = "")
         a1.id<-getURL(uid.call)
         if(length(c(na.omit(names(fromJSON(a1.id)[attribs]))))!=0){
            attr(mutuals.dat[[i]],c(na.omit(names(fromJSON(a1.id)[attribs]))))<-c(na.omit(fromJSON(a1.id)[attribs]))
         }
      }
   }
   names(mutuals.dat)<-unlist(friends.dat)
   attr(mutuals.dat,"v.names")<-unlist(lapply(fromJSON(friends.json)$data, function(x) x[[1]]))
   outpm<-vector("list",length=5)
   names(outpm)<-c("friends","mutuals","date","attribs","idtok")
   outpm$mutuals<-mutuals.dat
   if(ret.friends){
      outpm$friends<-do.call("rbind",fromJSON(friends.json)$data)
   }
   outpm$date<-format(Sys.time(), "%a %b %d %H:%M:%S %Y") 
   outpm$attribs<-attribs
   outpm$idtok<-c(myid,mytoken)
   class(outpm)<-"fbel"
   return(outpm)
}

#a print method for the above object
print.fbel<-function(x, ...){
   if(class(x)!="fbel"){stop("This is Not a Facebook Edgelist")}
   cat(paste(" Facebook user",x$idtok[1],"has",length(x$mutuals),"Facebook(tm) friends.",sep=" "),"\n")
   hd<-max(unlist(lapply(x$mutuals,length)))
   hdf.ind<-which(unlist(lapply(x$mutuals,length))%in%max(unlist(lapply(x$mutuals,length))))
   hdf<-names(x$mutuals)[hdf.ind]
   no.isos<-length(which(unlist(lapply(x$mutuals,function(x) any(is.na(x))))))
   cat(paste(" There are",no.isos,"isolated friends.",sep=" "),"\n",paste("Highest degree friend(s): ",hdf),"\n\t",paste("Highest Degree=",hd,".",sep=""),"\n")
   if(!is.null(x$attribs)){cat(" Friend attributes:",paste("`",x$attribs,"'",","," ",sep=""),"\n")}
   cat(paste(" This edgelist was downloaded on:",x$date,sep=" "),"\n", " Using token:",x$idtok[2],"\n")
   invisible(x)
   
   }

#convert the fbook object to a matrix
fbel2mat<-function(fbel){
   vnames<-names(fbel$mutuals)   
   if(any(vnames=="")){vnames[which(vnames=="")]<-NA}
   tmp.mat<-matrix(0,nrow=length(vnames),ncol=length(vnames))
   colnames(tmp.mat)<-vnames
   rownames(tmp.mat)<-vnames
   for(i in 1:length(vnames)){
      tmp.mat[i,fbel$mutuals[[i]]]<-1
   }
   return(tmp.mat)
}

#convert the fbook object to a network object
fbel2net<-function(fbel,v.attr=FALSE){
   fbmat<-fbel2mat(fbel)
   fb.net<-as.network(fbmat,directed=FALSE)
   if(v.attr){
      set.vertex.attribute(fb.net,"fbook.id",names(fbel$mutuals))
      attribs<-fbel$attribs
      if(!is.null(attribs)){
         attr.mat<-matrix(sapply(attribs,function(x) lapply(fbel$mutuals,function(y) attr(y,x))))
         for(i in 1:length(attribs)){
             set.vertex.attribute(fb.net,attribs[i],attr.mat[,i])
         }
      }
      if(!is.null(fbel$friends)){
         vnames<-fbel$friends[,1]
         set.vertex.attribute(fb.net,"real.name",vnames)
      }
   }   
   return(fb.net)
}

#extract single elements of a facebook graph, see api for definition of connection
get.facebook.connection<-function(myid,mytoken,connection,data.only=TRUE,...){
   fields<-c(...)
   if(!is.null(fields)){
      my.fields<-paste("?fields=",paste(fields,collapse=","),sep="")
      conn.call<-paste("https://graph.facebook.com/", myid, "/",connection,my.fields, "&access_token=", mytoken, sep = "")      
   } 
   else conn.call<-paste("https://graph.facebook.com/", myid, "/",connection, "?access_token=", mytoken, sep = "")
   conn.json<-getURL(conn.call)
   conn.dat<-fromJSON(conn.json)
   if(!is.null(conn.dat$error)){print(conn.dat$error)}
   if(data.only){return(do.call("rbind",conn.dat$data))}
   else return(conn.dat)
}

#collect meta information on a facebook graph, including all possible information available given the token access permissions
get.facebook.metadata<-function(myid,mytoken){
   meta.call<-paste("https://graph.facebook.com/", myid,"?metadata=1", "&access_token=", mytoken, sep = "")
   meta.json<-getURL(meta.call)
   meta.dat<-fromJSON(meta.json)
   class(meta.dat)<-"fbmeta"
   return(meta.dat)
}

#obtain all connections, all fields, or both all fields and all collections available given the token access permissions
get.facebook.data<-function(fbmeta,type=c("connection","field","both"),what=NULL,new.token=NULL){
   if(class(fbmeta)!="fbmeta"){stop("This is Not a Facebook Metadata Object")}
   type<-type[1]
   if(!is.null(new.token)){
    conn.lst<-strsplit(fbmeta$metadata$connections,split="access_token=")
    fbmeta$metadata$connections<-unlist(lapply(conn.lst,function(x) paste(x[1],"access_token=",new.token,sep="")))
   }
   if(type=="connection"){
      if(is.null(what)){what<-select.list(names(fbmeta$metadata$connection),multiple=TRUE,title="Select Connection Type")}
      conn.json<-getURL(fbmeta$metadata$connection[what])
      conn.dat<-list()
      for(i in 1:length(what)){
         conn.dat[[i]]<-fromJSON(conn.json[[i]])
      }
      names(conn.dat)<-what
      return(conn.dat)
   }    
   if(type=="field"){
      if(is.null(what)){what<-select.list(names(fbmeta)[-which(names(fbmeta)=="metadata")],multiple=TRUE,title="Select Field Type")}
      field.dat<-fbmeta[what]
      return(field.dat)
   }
   if(type=="both"){
      if(is.null(what)){
         what.conn<-select.list(names(fbmeta$metadata$connection),multiple=TRUE,title="Select Connection Type")
         what.fiel<-select.list(names(fbmeta)[-which(names(fbmeta)=="metadata")],multiple=TRUE,title="Select Field Type")
         what<-list(what.conn,what.fiel)
      }
      conn.json<-getURL(fbmeta$metadata$connection[what[[1]]])
      conn.dat<-list()
      for(i in 1:length(what[[1]])){
         conn.dat[[i]]<-fromJSON(conn.json[[i]])
      }
      names(conn.dat)<-what[[1]]
      field.dat<-fbmeta[what[[2]]]
      both.dat<-list(connections=conn.dat,fields=field.dat)
      return(both.dat)
   }
}

