library(grid);
getData = function()
{
	if(.Platform$OS.type == 'windows') x11() else x11(type = 'Xlib');
    pushViewport(viewport());
    grid.rect();
    px = NULL;
    py = NULL;
    mousedown = function(buttons, x, y)
    {
        if(length(buttons) > 1 || identical(buttons, 2L)) return(invisible(1));
        eventEnv$onMouseMove = mousemove;
        NULL
    }
    mousemove = function(buttons, x, y)
    {
        px <<- c(px, x);
        py <<- c(py, y);
        grid.points(x, y);
        NULL
    }
    mouseup = function(buttons, x, y) {    
        eventEnv$onMouseMove = NULL;
        NULL
    }
    setGraphicsEventHandlers(onMouseDown = mousedown,
                             onMouseUp = mouseup);
    eventEnv = getGraphicsEventEnv();
    cat("Click down left mouse button and drag to draw the number,
		right click to finish.\n");
    getGraphicsEvent();
    dev.off();
    s = seq(0, 1, length.out = length(px));
    spx = spline(s, px, n = 500)$y;
    spy = spline(s, py, n = 500)$y;
    return(cbind(spx, spy));
}

traceCorr = function(dat1, dat2)
{
    cor(dat1[, 1], dat2[, 1]) + cor(dat1[, 2], dat2[, 2]);
}

# Please set the proper path of this file.
load("C:/Users/bhargav/Downloads/Handwriting_recognition/train.RData");
guess = function(verbose = FALSE)
{
    test = getData();
    coefs = sapply(recogTrain, traceCorr, dat2 = test);
    num = which.max(coefs);
    if(num == 10) num = 0;
    if(verbose) print(coefs);
    cat("I guess what you have input is ", num, ".\n", sep = "");
}

