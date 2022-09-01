#
#  ********************************
#  **  Written by Yoan Diekmann  **
#  **   ydiekman@uni-mainz.de    **
#  ********************************
#
annot = read.table('data.tsv', quote = "", sep='\t',
                col.names = c('site_grave', 'ID', 'mean_date_BCE', 'dating', 'group_ID', 'country', 'lat', 'long',
                              'reference', 'steppe_prop', 'manual_steppe_prop', 'nb_snps', 'included', 'class',
                              'comment', 'manual_ancestry_chack', 'outlier'), skip = 1)
annot = annot[annot$mean_date_BCE<3500, ]
dat = annot[, c('ID', 'manual_steppe_prop')]
dat[is.na(dat$manual_steppe_prop), 2] = 0
dat = cbind(dat, as.numeric(sapply(annot[, 'class'], function(x) if (x=='BCB') 0 else 1)))
colnames(dat) = c('ID', 'manual_steppe_prop', 'class')
#
logreg = glm(class ~ manual_steppe_prop, family = binomial(link = "logit"), dat)
dat_ <- data.frame(manual_steppe_prop=seq(0, 1, len=500))
dat_$class = predict(logreg, dat_, type='response')
#
pdf(file = 'log_reg.pdf', width = 7, height = 5)
plot(NA, xlim=c(0, 1), ylim=c(0, 1), yaxt='n', xlab='steppe ancestry proportion', ylab='')
mtext(text = 'burial practice', side = 2, line = 2)
axis(side = 2, at = c(0, 1), labels = FALSE)
text(y = c(0, 1)+0.04, par('usr')[1]-0.02, labels = c('BCB', 'PCS'), srt = 45, pos = 2, xpd = TRUE)
#
xs = annot[which(annot$included==''), 'manual_steppe_prop']
points(jitter(xs), jitter(rep(1, length(xs))), col='black')
xs = annot[which(annot$included!='' & annot$class=='PCS'), 'manual_steppe_prop']
points(jitter(xs, amount=0.02), jitter(rep(1, length(xs))), col='black', pch=16)
#
xs = annot[which(annot$class=='BCB'), 'manual_steppe_prop']
points(jitter(xs, amount=0.015), jitter(rep(0, length(xs))), col='black', pch=16)
#
lines(class ~ manual_steppe_prop, dat_, lwd=1, lty=2)
#
par(xpd=TRUE)
legend('topright', inset=c(0, -0.19), legend=c('Yamnaya ref. ind.', 'logistic regression'),
       pch=c(1, NA), lwd=c(NA, 1), lty=c(NA, 2), col=c('black', 'black'), cex=0.8, seg.len=1.5)
#
dev.off()






