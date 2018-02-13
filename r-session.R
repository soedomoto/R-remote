args <- commandArgs(trailingOnly=TRUE);
if (length(args)==0) {
  stop("First argument as \'port\' must be supplied.", call.=FALSE)
}

port <- args[1];

library(rzmq)

context = init.context()
socket = init.socket(context, "ZMQ_REP")
bind.socket(socket, paste("tcp://*:", port, sep=""))

cat('[', port, ']', ' Session server is listening on port ', port, '\n');

while(1) {
    msg = receive.socket(socket);
    cmd <- msg$cmd;
    func <- msg$func;
    args <- msg$args;    
    resp <- NULL;

    if (!is.null(cmd)) { 
        if (grepl('<-', cmd)) {
            cat('[', port, ']', 'Executing command', cmd, '\n');

            eval(parse(text=cmd));
            resp <- 'OK';
        } else {
            cat('[', port, ']', 'Evaluating command', cmd, '\n');
            resp <- eval(parse(text=cmd));
        }
    } 

    else if (!is.null(func)) {
        cat('[', port, ']', 'Calling function', func, '\n');
        resp <- do.call(func, args);
    }
    
    send.socket(socket, resp);
}