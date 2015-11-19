var Channel = {
    emit: function(path, data) {
        return external.run('hydna', ['emit', path, data]);
    },
    send: function(path, data) {
        return external.run('hydna', ['send', path, data]);
    }
};
