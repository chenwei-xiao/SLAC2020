## Plot neural net:
plot_ann <- function(n_input, n_hidden, weights, offset_b_x=-0.5, offset_b_y=0.8,
                    loc_i_x=1, loc_h_x=3, loc_o_x=5, cex_val=4) {
    loc_i_x <- 1; loc_h_x <- 3; loc_o_x <- 5
    loc_h_y <- 1:n_hidden; loc_o_y <- mean(loc_h_y)
    loc_i_y <- 1:n_input; loc_i_y <- loc_i_y+mean(loc_h_y)-mean(loc_i_y)

    start_x <- c(rep(c(loc_h_x+offset_b_x, rep(loc_i_x, n_input)),n_hidden),
                 loc_o_x+offset_b_x, rep(loc_h_x,n_hidden))
    
    start_y <- c(c(t(matrix(c(loc_h_y+offset_b_y, 
                              c(sapply(1:n_input, function(i) rep(loc_i_y[i],n_hidden)))),
                              #rep(loc_i_y[1],n_hidden), rep(loc_i_y[2],n_hidden)),
                                       ncol=n_input+1))),
                 
                 loc_o_y+offset_b_y, loc_h_y)
    
    end_x <- c(rep(loc_h_x,(n_input*n_hidden+n_hidden)), rep(loc_o_x,n_hidden+1))
    end_y <- c(rep(loc_h_y,each=n_input+1), rep(loc_o_y,n_hidden+1))
    col_line_weights <- rep("blue",length(nn.fit.size5$wts))
    col_line_weights[nn.fit.size5$wts<0] <- "red"
    size_line_weights <- abs(nn.fit.size5$wts)

    points_x   <- c(rep(loc_i_x,n_input),rep(loc_h_x,n_hidden),
                    loc_o_x,rep(loc_h_x+offset_b_x,n_hidden),loc_o_x+offset_b_x)
    points_y   <- c(loc_i_y, loc_h_y, loc_o_y, loc_h_y+offset_b_y, loc_o_y+offset_b_y)
    points_col <- c(rep("orange",n_input), rep("gold",n_hidden), "darkolivegreen3",
                    rep("dodgerblue2",n_hidden+1))
    points_pch <- c(rep(21, n_input+n_hidden+1),rep(23, n_hidden+1))
    points_text <- c(paste0("i",1:n_input), paste0("h",1:n_hidden), "o", rep("b",n_hidden+1))

    plot(points_x, points_y, axes=F, xaxt='n', ann=FALSE)
    segments(x0=start_x, x1=end_x, y0=start_y, y1=end_y, lwd=size_line_weights, col=col_line_weights)
    points(points_x, points_y, col="black", bg=points_col, cex=cex_val, pch=points_pch)
    text(points_x, points_y, points_text, col="white")
    legend("topright", legend=c("Weights (pos)", "Weights (neg)", "Input nodes",
                                "Hidden layer nodes", "Ouput", "Bias"),
           col=c("blue","red",unique(points_col)), lwd=c(1,1,rep(NA,4)),
           pch=c(NA,NA,rep(16,3),18), bty="n")
}