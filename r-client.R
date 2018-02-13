library(rzmq)

remote.exec <- function(socket, func, ...) {
    send.socket(socket, data=list(func=func, args=list(...)))
    receive.socket(socket)
}

remote.eval <- function(socket, cmd) {
    send.socket(socket, data=list(cmd=cmd))
    receive.socket(socket)
}

remote.cleanup <- function(socket) {
    send.socket(socket, data=list(cmd='rm(list = setdiff(ls(), lsf.str()));'))
    receive.socket(socket)
}

# substitute(expr)
context = init.context()
socket = init.socket(context,"ZMQ_REQ")
connect.socket(socket,"tcp://localhost:5555")

# data.contoh <- remote.exec(socket, 'read.csv', 'contoh.csv', header=TRUE)
# data.contoh <- remote.exec(socket, subset, data.contoh, data.contoh[,"sumber"]=='ssn')
remote.eval(socket, 'data.contoh <- read.csv("contoh.csv",header=TRUE)');
# remote.cleanup(socket);
data.contohxs <- remote.eval(socket, 'data.contoh');
print(data.contohxs)