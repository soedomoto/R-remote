library(rzmq)
library(session)
library(uuid)

port <- 5000
server.port = 5555
context = init.context()
socket = init.socket(context, "ZMQ_REP")
bind.socket(socket, paste("tcp://*:", server.port))

cat('[', server.port, ']', 'Server is listening on port 5555\n')

session.port <- as.numeric(paste(port, 1, sep=""))
session.id.socket <- list()

remote.call <- function(r.socket, func, args) {
    send.socket(r.socket, data=list(func=func, args=args))
    receive.socket(r.socket)
}

remote.eval <- function(r.socket, cmd) {
    send.socket(r.socket, data=list(cmd=cmd))
    receive.socket(r.socket) 
}

init.session <- function(session.uuid=NULL) {
    if (is.null(session.uuid)) {
        session.uuid = UUIDgenerate(use.time = TRUE)
    }

    if (!is.null(session.id.socket[[ session.uuid ]])) {
        return(session.id.socket[[ session.uuid ]])
    }

    broker.rep.port = 50011
    broker.rep.uri = paste("tcp://localhost:", broker.rep.port, sep="")
    broker.rep.context = init.context()
    broker.rep.socket = init.socket(broker.rep.context, "ZMQ_REP")
    bind.socket(broker.rep.socket, paste("tcp://*:", broker.rep.port, sep=""))

    cat('[', server.port, ']', 'Waiting for server session', session.uuid, 'response...\n')

    cmd <- paste('Rscript', './r-session.R', session.port, session.uuid, broker.rep.uri, sep=" ")
    cat('[', server.port, ']', 'Executing', cmd, '\n')
    system(cmd, wait=FALSE)

    session.pong = receive.socket(broker.rep.socket)
     if (!is.null(session.pong)) {
        disconnect.socket(broker.rep.socket, broker.rep.uri)

        broker.req.context = init.context()
        broker.req.socket = init.socket(broker.req.context,"ZMQ_REQ")        
        if (connect.socket(broker.req.socket, paste("tcp://localhost:", session.port, sep=""))) {
            cat('[', server.port, ']', 'Session server', session.uuid, 'now reachable\n')

            session.port <<- session.port + 1
            session.id.socket[[ session.uuid ]] <<- broker.req.socket
            return(broker.req.socket)
        }
     }
}

while(1) {
    msg = receive.socket(socket);
    sess <- msg$sess;
    cmd <- msg$cmd;
    func <- msg$func;
    args <- msg$args;    
    resp <- 'Failed';
    
    socket.session <- init.session(sess)

    if (!is.null(cmd)) {
        resp <- remote.eval(socket.session, cmd)
    } 

    else if (!is.null(func)) {
        resp <- remote.call(socket.session, func, args);
    }
    
    send.socket(socket, resp);
}