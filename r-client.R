library(rzmq)

remote.new.session <- function(socket) {
    send.socket(socket, data=list(func='init.session', args=list()))
    receive.socket(socket)
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

context = init.context()
socket = init.socket(context,"ZMQ_REQ")
connect.socket(socket,"tcp://localhost:5555")

sess.id <- remote.new.session(socket)
# sess.id <- 'eba7fdca-10c7-11e8-a38f-1008b155cbfd'
print(sess.id)


# data.contoh <- remote.call(socket, sess.id, 'read.csv', 'data/contoh.csv', header=TRUE)
# data.contoh <- remote.call(socket, sess.id, 'subset', data.contoh, data.contoh[,"sumber"]=='ssn')
# print(data.contoh)

remote.eval(socket, sess.id, 'data.contoh <- read.csv("data/contoh.csv",header=TRUE)');
remote.eval(socket, sess.id, 'data.contoh <- subset(data.contoh, data.contoh[,"sumber"]=="ssn")');
data.contoh <- remote.eval(socket, sess.id, 'data.contoh');
print(data.contoh)