constraint 1 [y1]b2 = [y2]b2
constraint 2 [y1]b3 = [y2]b3
constraint 3 [y1]b4 = [y2]b4
constraint 4 [y1]b5 = [y2]b5
constraint 5 [y1]b6 = [y2]b6


sureg (y1 var2-var7 b2-b6) (y2 bar2-bar7 b2-b6), const(1 2 3 4 5)


* OLS
reg(y1 var2-var7 b2-b6)
reg(y2 bar2-bar7 b2-b6)


reg3 (y1 var2-var7 b2-b6) (y2 bar2-bar7 b2-b6),  const(1 2 3 4 5) ols



reg y1 var2-var7 b2-b6


constraint 1 [allothers]acer_rwpsprice = [compaq]acer_rwpsprice
constraint 2 [hp]acer_rwpsprice = [compaq]acer_rwpsprice

constraint 3 [allothers]acer_llength = [compaq]acer_llength
constraint 4 [hp]acer_llength = [compaq]acer_llength

constraint 5 [allothers]acer_wpswdist1 = [compaq]acer_wpswdist1
constraint 6 [hp]acer_wpswdist1 = [compaq]acer_wpswdist1

constraint 7 [allothers]acer_novel1 = [compaq]acer_novel1
constraint 8 [hp]acer_novel1 = [compaq]acer_novel1

constraint 9 [allothers]acer_ylag_1 = [compaq]acer_ylag_1
constraint 10 [hp]acer_ylag_1 = [compaq]acer_ylag_1

reg3 (allothers allothers_* acer_*) (compaq compaq_* acer_*)  (hp hp_* acer_*) ,  const(1 2 3 4 5 6 7 8 9 10) ols




sureg (allothers allothers_* acer_*) (compaq compaq_* acer_*)  (hp hp_* acer_*) ,  const(1 2 3 4 5 6 7 8 9 10)





* new v


constraint 1 [allothers]acer_rwpsprice = [compaq]acer_rwpsprice

constraint 3 [allothers]acer_llength = [compaq]acer_llength

constraint 5 [allothers]acer_wpswdist1 = [compaq]acer_wpswdist1

constraint 7 [allothers]acer_novel1 = [compaq]acer_novel1

constraint 9 [allothers]acer_ylag_1 = [compaq]acer_ylag_1

reg3 (allothers allothers_* acer_*) (compaq compaq_* acer_*),  const(1 3 5 7 9) ols

