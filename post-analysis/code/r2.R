# fit

cor(dv,predict(m))^2
[1] 0.004528109
> cor(dv*1/tmp$elastlt_se,predict(m))^2
[1] 0.001036899
> cor(dv*(1/tmp$elastlt_se),predict(m))^2
[1] 0.001036899
> cor(dv*(tmp$elastlt_se),predict(m))^2
[1] 0.003117176
> cor(tmp$elastlt*(1/tmp$elastlt_se),predict(m))^2
[1] 0.001036899
> ?predict

tmp=elast[variable=='rwpspr']

mw=lm(elastlt~1+emerging,data=tmp,weights=1/elastlt_se)
m=lm(elastlt~1+emerging,data=tmp)
mx2=lm(I(elastlt/elastlt_se)~1+emerging,data=tmp)


m2=lmer(elastlt~1+emerging+(1|category),data=tmp)

res=data.table(tmp$elastlt, predict(m),predict(mw, weights=rep(1,nrow(tmp)), data=tmp))
cor(res)^2


rsq <- function(m) {
  f=predict(m)
  r=resid(m)

  if (class(m)=='lm') {
    if (is.null(m$weights)) w=rep(1,length(f))
  }
  if (class(m)=='lmerMod') {
    w=weights(m)
  }
  
  #  mss=sum((f - mean(f))^2)
   # rss=sum(r^2)
  #} else {
   # w=m$weights
    mx <- sum(w * f/sum(w))
    mss=sum(w * (f - mx)^2)
    rss <- sum(w * r^2)
  #}
  
  mss/(mss+rss)
  
}

rsq <- function(m) {
  f=predict(m)
  r=resid(m)
  cor(f, f+r)^2
}

# squared correlation does not work (!)
