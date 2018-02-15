library(rzmq)
source('r-api.R')

context = init.context()
socket = init.socket(context,"ZMQ_REQ")
connect.socket(socket,"tcp://localhost:5000")

# Use any session ID
sess.id <- '87d7b602-10ca-11e8-b177-1008b155cbfq'

#Sequence of commands
remote.eval(socket, sess.id, 'data.contoh <- read.csv(data.path("contoh.csv"),header=TRUE)')
remote.eval(socket, sess.id, 'data.contoh <- subset(data.contoh, data.contoh[,"sumber"]=="ssn")')
remote.eval(socket, sess.id, 'data.pop <- read.csv(data.path("populasi.csv"),header=TRUE)')
remote.eval(socket, sess.id, 'y.contoh <- data.contoh$perkapita')
remote.eval(socket, sess.id, 'x.contoh <- cbind(data.contoh$x0,data.contoh$art,data.contoh$lap_usaha,data.contoh$status_usaha)')
remote.eval(socket, sess.id, 'vektor.kodearea.contoh <- data.contoh$kec')
remote.eval(socket, sess.id, 'x.populasi <- cbind(data.pop$x0,data.pop$art,data.pop$lap_usaha,data.pop$status_usaha)')
remote.eval(socket, sess.id, 'vektor.kodearea.populasi <- data.pop$kec')
remote.eval(socket, sess.id, 'vektor.kodearea.populasi <- data.pop$kec')
remote.eval(socket, sess.id, 'fungsi.lengkap(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo=50,boot.populasi=1,boot.contoh=100,garis.kemiskinan = 342956)')