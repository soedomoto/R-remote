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