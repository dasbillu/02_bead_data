
bd.check_similarity <- function(data, params, which.params, m, l, u, anchor=NULL, ids) {
  pacman::p_load(tidyverse)
  foo <- data;
    if(!is.null(anchor)) {
      all.letters <- list()
      for(i in 1:length(which.params)) {
        bar1 <-
          foo %>% 
          select({{params}},{{m}},{{l}},{{u}},{{anchor}},{{ids}})
        bar1 <- bar1[which(bar1[1]==which.params[i]),]
        colnames(bar1) <- c("p","m","l","u","anchor","id")
        bar2 <-
          bar1 %>% 
          inner_join(bar1, by=c("anchor")) %>% 
          mutate(c = ifelse(m.x==m.y, "ns",
                            ifelse(m.y>=l.x & m.y<=u.x | m.x>=l.y & m.x<=u.y, 
                                   "ns","sig"))) %>% 
          group_by(anchor, id.x, id.y) %>% 
          mutate(ids = paste(sort(c(id.x,id.y)),collapse = "-")) %>% 
          ungroup() %>% 
          select(anchor, ids , c) %>% 
          distinct() %>% 
          mutate(id2=ids) %>% 
          separate(id2, c("id1","id2"), "-")
        
        which.anchor <- unique(bar1$anchor)
        which.ids <- unique(bar1$id)
        l.letters <- list()
        for (j in 1:length(which.anchor)) {
          dif3 <- bar2 %>% filter(anchor==which.anchor[j]) %>% mutate(c=ifelse(c=="ns",F,T)) %>% pull(c)
          names(dif3) <- bar2 %>% filter(anchor==which.anchor[j]) %>% pull(ids)
          dif3L <- multcompView::multcompLetters(dif3)
          l.letters[[j]] <-
            data.frame(id = which.ids,
                       anchor = which.anchor[j],
                       params=which.params[i],
                       letters = dif3L$Letters)
          colnames(l.letters[[j]]) <- c(ids, anchor, params, "letters")
        }
        all.letters[[i]] <- do.call(rbind,l.letters)
      }
      return(do.call(rbind,all.letters))
      
    } else {
    writeLines("Please specify an anchor column.")
    }  
}


# ## An example
# 
# ## Data from Alicia
# load("/Users/biplabendudas/Desktop/Alicia/11Nov22/foo.rdata")
# foo2 <- foo
# foo <-
#   foo %>% 
#   group_by(params) %>% 
#   mutate(params2 = str_split(params, "\\[")[[1]][1]) %>% 
#   select(mean=mean,
#          low=lower2.5,
#          upp=upper97.5,
#          params, params2,
#          run) %>% 
#   group_by(run,params2) %>% 
#   mutate(id=row_number()) %>% 
#   ungroup()
# 
# 
# res <- 
#   rbind(bd.check_similarity(data = foo,
#                     params = "params2",
#                     which.params = c("alpha.star","alpha","sig", "pA.pred", "pD.pred"),
#                     m="mean",
#                     l="low",
#                     u="upp",
#                     anchor = "id",
#                     ids = "run"),
#       bd.check_similarity(data = foo,
#                           params = "params2",
#                           which.params = c("beta"),
#                           m="mean",
#                           l="low",
#                           u="upp",
#                           anchor = "run",
#                           ids = "id")
# )
# 
# foo %>%
#   ungroup() %>%
#   # mutate(id=as.character(id),
#   #        run=as.character(run)) %>%
#   left_join(res, by=c("id","run","params2")) %>%
#   select(params, params2, letters) %>%
#   right_join(foo2, by=c("params")) %>%
#   select(-run)
# 
# 
# 
