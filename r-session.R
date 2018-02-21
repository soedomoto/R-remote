args <- commandArgs(trailingOnly=TRUE)
if (length(args)<2) {
  stop("1st argument as \'port\' and 2nd argument as \'session id\' must be supplied.", call.=FALSE)
}

script.dir <- function(fname){
    args.local <- commandArgs(trailingOnly = F)  
    return(normalizePath(dirname(sub("^--file=", "", args.local[grep("^--file=", args.local)]))))
}

session.port <- args[1]
session.id <- args[2]
broker.uri <- args[3]
session.workspace <- paste(script.dir(), '/workspace/', session.id, sep='')
session.filename <- paste('session.Rdata', sep='')

# SET WORKSPACE
if (!dir.exists(session.workspace)) {
    dir.create(session.workspace, showWarnings = FALSE, recursive = TRUE)
}

# RESTORING SESSION
setwd(session.workspace)
if (file.exists(session.filename)) {
    cat('[', session.port, ']', 'Restoring session', session.filename, '\n')
    load(file=session.filename)

    # Sometimes after restoring session, the variable is changed
    # So, reassign variables from cmd args
    args <- commandArgs(trailingOnly=TRUE)
    session.port <- args[1]
    session.id <- args[2]
    broker.uri <- args[3]

    cat('[', session.port, ']', 'Session', session.filename, 'is restored\n')
} 

# Load all available modules (*.R)
file.sources = list.files(c('../../modules'), pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

# STARTING SERVER ....
library(rzmq)

session.context.out = init.context()
session.socket.out = init.socket(session.context.out, "ZMQ_REP")
bind.socket(session.socket.out, paste("tcp://*:", session.port, sep=""))
cat('[', session.port, ']', 'Session server', session.id, 'is listening on session.port ', session.port, '\n');

# SEND HEARTBEAT TO BROKER
session.context.heartbeat = init.context()
session.socket.heartbeat = init.socket(session.context.heartbeat,"ZMQ_REQ")        
if (connect.socket(session.socket.heartbeat, broker.uri)) {
    cat('[', session.port, ']', 'Notifying broker', '\n');
    send.socket(session.socket.heartbeat, data=list(str='pong'))
}

save.image(file=session.filename)

# READY TO ACCEPT MESSAGE
while(1) {
    cat('[', session.port, ']', 'Reading message...', '\n');

    data = receive.socket(session.socket.out);
    cmd <- data$cmd;
    func <- data$func;
    args <- data$args;    
    resp <- 'Failed';

    if (!is.null(cmd)) { 
        if (grepl('<-', cmd)) {
            cat('[', session.port, ']', 'Executing command', cmd, '\n');

            eval(parse(text=cmd));
            resp <- 'OK';
        } else {
            cat('[', session.port, ']', 'Evaluating command', cmd, '\n');
            resp <- eval(parse(text=cmd));
        }
    } 

    else if (!is.null(func)) {
        cat('[', session.port, ']', 'Calling function', func, '\n');
        resp <- do.call(func, args);
    }
    
    send.socket(session.socket.out, resp);

    # Save session for next purpose
    save.image(file=session.filename)
}