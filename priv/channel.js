var Channel = {
    emit: function(path, data) {
        return external.run('hydna', ['emit', path, data]);
    }
};
