library(rzmq)

context = init.context()
socket = init.socket(context, "ZMQ_REP")
bind.socket(socket, "tcp://*:5555")

print('Server is listening on port 5555');

while(1) {
    msg = receive.socket(socket);
    cmd <- msg$cmd;
    func <- msg$func;
    args <- msg$args;    
    resp <- NULL;

    if (!is.null(cmd)) { 
        if (grepl('<-', cmd)) {
            cat('Executing command', cmd, '\n');

            eval(parse(text=cmd));
            resp <- 'OK';
        } else {
            cat('Evaluating command', cmd, '\n');
            resp <- eval(parse(text=cmd));
        }
    } 

    else if (!is.null(func)) {
        cat('Calling function', func, '\n');
        resp <- do.call(func, args);
    }
    
    send.socket(socket, resp);
}