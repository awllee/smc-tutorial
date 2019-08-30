library(ggplot2)
library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

make.indices.plot <- function(n, N) {
  df.indices <- data.frame()
  for (p in 1:n) {
    df.indices <- rbind(df.indices, data.frame(time=p, index=1:N))
  }
  df.indices <- df.indices %>% accumulate_by(~time)
  g <- ggplot(df.indices, aes(x=time, y=index, frame=frame)) + geom_point()
  return(g)
}

make.ancestors.plot <- function(as) {
  n <- dim(as)[1]+1
  N <- dim(as)[2]
  
  df.ancestors <- data.frame()
  df.ancestors <- rbind(df.ancestors, data.frame(time=1, index=1:N, ancestor=1:N))
  for (p in 1:(n-1)) {
    df.ancestors <- rbind(df.ancestors, data.frame(time=p+1, index=1:N, ancestor=as[p,]))
  }
  
  df.ancestors <- df.ancestors %>% accumulate_by(~time)
  
  g <- make.indices.plot(n, N)
  ga <- g + geom_segment(aes(x = pmax(1,time-1), y = ancestor, xend = time, yend = index, colour = "ancestor"),
                         data = df.ancestors)

  return(ga)
}

make.lineages.plot <- function(as, ix) {
  n <- dim(as)[1]+1
  N <- dim(as)[2]
  
  df.descendants <- data.frame()
  for (p in (n-1):1) {
    df.descendants <- rbind(df.descendants, data.frame(time=p+1, index=ix, ancestor=as[p,ix]))
    ix <- unique(as[p,ix])
  }
  df.descendants <- rbind(df.descendants, data.frame(time=1, index=ix, ancestor=ix))
  df.descendants <- df.descendants %>% accumulate_by(~time)
  
  ga <- make.ancestors.plot(as)
  gad <- ga + geom_segment(aes(x = pmax(1,time-1), y = ancestor, xend = time, yend = index, colour = "survived"),
                           data = df.descendants)
  return(gad)
}

make.particle.plot <- function(xs, as) {
  n <- dim(xs)[1]
  N <- dim(xs)[2]
  
  df.particles <- data.frame()
  for (p in 1:n) {
    df.particles <- rbind(df.particles, data.frame(time=p, x=xs[p,]))
  }
  
  df.ancestors <- data.frame()
  df.ancestors <- rbind(df.ancestors, 
                        data.frame(time=1, index=1:N, ancestor=1:N,
                                   loc=xs[1,], anc.loc = xs[1,]))
  for (p in 1:(n-1)) {
    df.ancestors <- rbind(df.ancestors,
                          data.frame(time=p+1, index=1:N, ancestor=as[p,],
                                     loc=xs[p+1,], anc.loc = xs[p,as[p,]]))
  }
  
  df.particles <- df.particles %>% accumulate_by(~time)
  df.ancestors <- df.ancestors %>% accumulate_by(~time)
  
  
  
  g <- ggplot(df.particles, aes(x=time, y=x)) + geom_point(aes(frame=frame))
  
  ga <- g + geom_segment(aes(x = pmax(1,time-1), y = anc.loc, xend = time, yend = loc,
                             frame=frame, colour = "ancestral line"),
                         data = df.ancestors)
  return(ga)
}

make.particle.lineages.plot <- function(xs, as, ix=1:dim(xs)[2]) {
  n <- dim(xs)[1]
  N <- dim(xs)[2]
  
  df.descendants <- data.frame()

  for (p in (n-1):1) {
    df.descendants <- rbind(df.descendants,
                            data.frame(time=p+1, index=ix, ancestor=as[p,ix],
                                       loc=xs[p+1,ix], anc.loc = xs[p,as[p,ix]]))
    ix <- unique(as[p,ix])
  }
  df.descendants <- rbind(df.descendants,
                          data.frame(time=1, index=ix, ancestor=ix,
                                     loc=xs[1,ix], anc.loc = xs[1,ix]))
  
  ga <- make.particle.plot(xs, as)
  gad <- ga + geom_segment(aes(x = pmax(1,time-1), y = anc.loc, xend = time, yend = loc,
                               colour = "survived"), data = df.descendants)
  return(gad)
}
