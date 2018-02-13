library(rzmq)
library(uuid)

port <- 5000
context = init.context()
socket = init.socket(context, "ZMQ_REP")
bind.socket(socket, "tcp://*:5555")

print('Server is listening on port 5555')

session.port <- as.numeric(paste(port, 1, sep=""))
session.id.socket <- list()

init.session <- function() {
    session.uuid = UUIDgenerate(use.time = TRUE)

    cmd <- paste('Rscript', './r-session.R', session.port, session.uuid, sep=" ")
    cat('Executing', cmd, '\n')
    system(cmd, wait=FALSE)

    context = init.context()
    socket = init.socket(context,"ZMQ_REQ")
    connect.socket(socket, paste("tcp://localhost:", session.port, sep=""))
    session.id.socket[[ session.uuid ]] <<- socket

    session.port <<- session.port + 1
    return(session.uuid)
}

remote.call <- function(socket, func, args) {
    send.socket(socket, data=list(func=func, args=args))
    receive.socket(socket)
}

remote.eval <- function(socket, cmd) {
    send.socket(socket, data=list(cmd=cmd))
    receive.socket(socket) 
}

while(1) {
    msg = receive.socket(socket);
    sess <- msg$sess;
    cmd <- msg$cmd;
    func <- msg$func;
    args <- msg$args;    
    resp <- 'Failed';

    if (!is.null(sess)) {
        socket.session <- session.id.socket[[sess]]
    }

    if (!is.null(cmd)) { 
        resp <- remote.eval(socket.session, cmd)
    } 

    else if (!is.null(func)) {
        if ('init.session' == func) {
            resp <- do.call(func, args);
            Sys.sleep(2);
        } else {
            resp <- remote.call(socket.session, func, args);
        }
    }
    
    send.socket(socket, resp);
}