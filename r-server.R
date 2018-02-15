library(rzmq)
library(session)
library(uuid)

template.port <- 5
broker.port <- as.numeric(paste(template.port, '000', sep="")) # e.g: 5000
session.port <- as.numeric(paste(template.port, '100', sep="")) # e.g: 5100
session.hearbeat.server.port <- as.numeric(paste(template.port, '600', sep="")) # e.g: 5600

session.uuid.socket.map <- list()

# STARTING SERVER ....
broker.context.out = init.context()
broker.socket.out = init.socket(broker.context.out, "ZMQ_REP")
bind.socket(broker.socket.out, paste("tcp://*:", broker.port))
cat('[', broker.port, ']', 'Server is listening on port', broker.port, '\n')

# GET SESSION SOCKET, CREATE IF NOT EXISTS
fn.remote.session <- function(session.uuid=NULL) {
    if (is.null(session.uuid)) {
        session.uuid = UUIDgenerate(use.time = TRUE)
    }

    # Return if there is already exists socket associated with session.uuid
    if (!is.null(session.uuid.socket.map[[ session.uuid ]])) {
        return(session.uuid.socket.map[[ session.uuid ]])
    }

    # Create if there is no socket associated with session.uuid
    # Starting heartbeat server 
    session.hearbeat.server.uri = paste("tcp://localhost:", session.hearbeat.server.port, sep="")
    session.hearbeat.server.context = init.context()
    session.hearbeat.server.socket = init.socket(session.hearbeat.server.context, "ZMQ_REP")
    bind.socket(session.hearbeat.server.socket, paste("tcp://*:", session.hearbeat.server.port, sep=""))

    cat('[', broker.port, ']', 'Waiting for server session', session.uuid, 'heartbeat...\n')

    # Invoke session server
    cmd <- paste('Rscript', './r-session.R', session.port, session.uuid, session.hearbeat.server.uri, sep=" ")
    cat('[', broker.port, ']', 'Executing', cmd, '\n')
    system(cmd, wait=FALSE)

    # Waiting for heartbeat
    session.pong = receive.socket(session.hearbeat.server.socket)
     if (!is.null(session.pong)) {
        # If heartbeat received, kill the hearbeat server
        # disconnect.socket(session.hearbeat.server.socket, session.hearbeat.server.uri)
        session.hearbeat.server.port <<- session.hearbeat.server.port + 1

        # Create new socket
        broker.req.context = init.context()
        broker.req.socket = init.socket(broker.req.context,"ZMQ_REQ")        
        if (connect.socket(broker.req.socket, paste("tcp://localhost:", session.port, sep=""))) {
            cat('[', broker.port, ']', 'Session server', session.uuid, 'now reachable\n')

            session.port <<- session.port + 1
            session.uuid.socket.map[[ session.uuid ]] <<- broker.req.socket
            return(broker.req.socket)
        }
     }
}

while(1) {
    data = receive.socket(broker.socket.out);
    sess.uuid <- data$sess;
    resp <- 'Failed';
    
    # Get appropriate socket for specific session server
    session.socket <- fn.remote.session(sess.uuid)
    # Redirect data to specific session server
    send.socket(session.socket, data=data)
    # Redirect client to client
    resp <- receive.socket(session.socket)     
    send.socket(broker.socket.out, resp);
}