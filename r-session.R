args <- commandArgs(trailingOnly=TRUE);
if (length(args)<2) {
  stop("1st argument as \'port\' and 2nd argument as \'session id\' must be supplied.", call.=FALSE)
}

port <- args[1]
sess.id <- args[2]
broker.uri <- args[3]

# STARTING SERVER ....
library(rzmq)
library(session)

context = init.context()
socket = init.socket(context, "ZMQ_REP")
bind.socket(socket, paste("tcp://*:", port, sep=""))

cat('[', port, ']', 'Session server', sess.id, 'is listening on port ', port, '\n');

if (file.exists(paste("session/", sess.id, ".Rdata", sep=""))) {
    cat('[', port, ']', 'Restoring session', paste("session/", sess.id, ".Rdata", sep=""), '\n')
    load(file=paste("session/", sess.id, ".Rdata", sep=""))
    cat('[', port, ']', 'Session', paste("session/", sess.id, ".Rdata", sep=""), 'is restored\n')
}

# SEND NOTIFY TO BROKER
broker.context = init.context()
broker.socket = init.socket(broker.context,"ZMQ_REQ")        
if (connect.socket(broker.socket, broker.uri)) {
    cat('[', port, ']', 'Notifying broker', '\n');
    send.socket(broker.socket, data=list(str='pong'))
}


# READY TO ACCEPT MESSAGE
while(1) {
    cat('[', port, ']', 'Reading message...', '\n');

    msg = receive.socket(socket);
    cmd <- msg$cmd;
    func <- msg$func;
    args <- msg$args;    
    resp <- 'Failed';

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

    if (!is.null(sess.id)) { 
        dir.create("session/", showWarnings = FALSE, recursive = TRUE)
        save.image(file=paste("session/", sess.id, ".Rdata", sep=""))
    }
}