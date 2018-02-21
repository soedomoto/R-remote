library(rzmq)

client.socket <- NULL
remote.connect <- function(addr, sess) {
    context = init.context()
    socket = init.socket(context,"ZMQ_REQ")
    connect.socket(socket, addr)

    send.socket(socket, data=list(sess=sess))
    sess.addr <- receive.socket(socket)
    disconnect.socket(socket, addr)

    if (!is.null(client.socket)) {
        return(client.socket)
    }

    context = init.context()
    client.socket = init.socket(context,"ZMQ_REQ")
    connect.socket(client.socket, sess.addr)
    return(client.socket)
}

remote.call <- function(socket, sess, func, ...) {
    send.socket(socket, data=list(func=func, args=list(...), sess=sess))
    receive.socket(socket)
}

remote.eval <- function(socket, sess, cmd) {
    send.socket(socket, data=list(cmd=cmd, sess=sess))
    receive.socket(socket)
}

remote.cleanup <- function(socket) {
    send.socket(socket, data=list(cmd='rm(list = setdiff(ls(), lsf.str()));'))
    receive.socket(socket)
}