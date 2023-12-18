dental_chart = function(col, labs=NULL, add=F, lims=NULL, 
                        inner_labels=T, outer_labels=T, main=NULL, 
                        cex=1, cex.lab=1, cex.surf=1, cex.main=1) {
  
  stopifnot(length(col) == 28)
  stopifnot(all(lapply(col, length) == 
                  c(5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 
                    5, 5, 5, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5)))
  if (!add) {
    plot.new()
  }
  if (is.null(lims)) {
    lims = par('usr')
  }  
  sep = 0.2
  width = (lims[2] - lims[1])/(14 + 15*sep)
  height = (lims[4] - lims[3])/(4 + 4*sep)
  lwd = cex
  for (r in 1:2) {
    for (c in 1:14) {
      index = 14*(r - 1) + c
      # External coordinates
      le = ((c - 1)*(1 + sep) + sep)/(14 + 15*sep)*(lims[2] - lims[1]) + lims[1]
      ri = le + width
      hi = lims[4] - height*(.5 + r)*(1 + sep)
      lo = hi - height
      
      if (c > 7) {
        tmp = ri
        ri = le
        le = tmp
      }

      if (r == 2) {
        tmp = lo
        lo = hi
        hi = tmp
      }
      tooth_cols = col[[index]]
      
      # Molars
      if (c %in% c(1:4, 11:14)) {
        # Internal coordinates
        ile = (2*le + ri)/3
        iri = (le + 2*ri)/3
        ilo = (2*lo + hi)/3
        ihi = (lo + 2*hi)/3
        polygon(c(le, ri, iri, ile, le), c(hi, hi, ihi, ihi, hi), col=tooth_cols[1], lwd=lwd) # B
        polygon(c(le, ile, ile, le, le), c(lo, ilo, ihi, hi, lo), col=tooth_cols[2], lwd=lwd) # D
        polygon(c(le, ri, iri, ile, le), c(lo, lo, ilo, ilo, lo), col=tooth_cols[3], lwd=lwd) # L
        polygon(c(ri, ri, iri, iri, ri), c(lo, hi, ihi, ilo, lo), col=tooth_cols[4], lwd=lwd) # M
        rect(ile, ilo, iri, ihi, col=tooth_cols[5], lwd=lwd) # O
        if (inner_labels) {
          text(x=mean(c(le, ri)), y=mean(c(ihi, hi)), labels=names(tooth_cols)[1], cex=0.75*cex*cex.surf) # B
          text(x=mean(c(le, ile)), y=mean(c(lo, hi)), labels=names(tooth_cols)[2], cex=0.75*cex*cex.surf) # D
          text(x=mean(c(le, ri)), y=mean(c(lo, ilo)), labels=names(tooth_cols)[3], cex=0.75*cex*cex.surf) # L
          text(x=mean(c(iri, ri)), y=mean(c(lo, hi)), labels=names(tooth_cols)[4], cex=0.75*cex*cex.surf) # M
          text(x=mean(c(ile, iri)), y=mean(c(ilo, ihi)), labels=names(tooth_cols)[5], cex=0.75*cex*cex.surf) # O
        }
      
      # Canines
      } else if (c %in% c(5, 10)) {
        # Internal coordinates
        ix = mean(c(le, ri))
        iy = mean(c(lo, hi))
        polygon(c(le, ri, ix, le), c(hi, hi, iy, hi), col=tooth_cols[1], lwd=lwd) # B
        polygon(c(le, ix, le, le), c(lo, iy, hi, lo), col=tooth_cols[2], lwd=lwd) # D
        polygon(c(le, ri, ix, le), c(lo, lo, iy, lo), col=tooth_cols[3], lwd=lwd) # L
        polygon(c(ri, ix, ri, ri), c(lo, iy, hi, lo), col=tooth_cols[4], lwd=lwd) # M
        if (inner_labels) {
          text(x=mean(c(le, ri)), y=(2*iy + 3*hi)/5, labels=names(tooth_cols)[1], cex=0.75*cex*cex.surf) # B
          text(x=(3*le + 2*ix)/5, y=mean(c(lo, hi)), labels=names(tooth_cols)[2], cex=0.75*cex*cex.surf) # D
          text(x=mean(c(le, ri)), y=(2*iy + 3*lo)/5, labels=names(tooth_cols)[3], cex=0.75*cex*cex.surf) # L
          text(x=(3*ri + 2*ix)/5, y=mean(c(lo, hi)), labels=names(tooth_cols)[4], cex=0.75*cex*cex.surf) # M
        }
      
      # Incisors
      } else if (c %in% (6:9)) {
        # Internal coordinates
        ile = (.65*le + .35*ri)
        iri = (.35*le + .65*ri)
        iy = (lo + hi)/2
        polygon(c(le, ri, iri, ile, le), c(hi, hi, iy, iy, hi), col=tooth_cols[1], lwd=lwd) # B
        polygon(c(le, le, ile, le), c(lo, hi, iy, lo), col=tooth_cols[2], lwd=lwd) # D
        polygon(c(le, ri, iri, ile, le), c(lo, lo, iy, iy, lo), col=tooth_cols[3], lwd=lwd) # L
        polygon(c(ri, ri, iri, ri), c(hi, lo, iy, hi), col=tooth_cols[4], lwd=lwd) # M
        if (inner_labels) {
          text(x=mean(c(le, ri)), y=mean(c(iy, hi)), labels=names(tooth_cols)[1], cex=0.75*cex*cex.surf) # B
          text(x=(.55*le + .45*ile), y=mean(c(lo, hi)), labels=names(tooth_cols)[2], cex=0.75*cex*cex.surf) # D
          text(x=mean(c(le, ri)), y=mean(c(lo, iy)), labels=names(tooth_cols)[3], cex=0.75*cex*cex.surf) # L
          text(x=(.55*ri + .45*iri), y=mean(c(lo, hi)), labels=names(tooth_cols)[4], cex=0.75*cex*cex.surf) # M
        }
      }
      if (outer_labels) {
        text(x=mean(c(le, ri)), y=ifelse(r == 2, hi - sep*height, hi + sep*height),
             labels=ifelse(is.null(names(col)), sprintf('%d:%d', r, c), names(col)[index]), 
             cex=cex*cex.lab, adj=c(.5, ifelse(r == 1, 0, 1)))
      }
    }
  }
  if (!is.null(main)) {
    text(x=mean(lims[1:2]), y=mean(lims[3:4]) + (1 + sep)*height, labels=main, cex=2*cex*cex.main, adj=c(.5, 0))
  }
}
