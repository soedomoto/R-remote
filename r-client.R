library(rzmq)
source('r-api.R')

context = init.context()
socket = init.socket(context,"ZMQ_REQ")
connect.socket(socket,"tcp://localhost:5555")

sess.id <- '87d7b602-10ca-11e8-b177-1008b155cbft'
print(sess.id)


# data.contoh <- remote.call(socket, sess.id, 'read.csv', 'data/contoh.csv', header=TRUE)
# data.contoh <- remote.call(socket, sess.id, 'subset', data.contoh, data.contoh[,"sumber"]=='ssn')
# print(data.contoh)

remote.eval(socket, sess.id, 'data.contoh <- read.csv("data/contoh.csv",header=TRUE)');
remote.eval(socket, sess.id, 'data.contoh <- subset(data.contoh, data.contoh[,"sumber"]=="ssn")');
data.contoh <- remote.eval(socket, sess.id, 'data.contoh');
print(data.contoh)