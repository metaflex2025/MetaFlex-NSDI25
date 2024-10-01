var dojoConfig = {
    async: true
}

require.config({
    packages : [
    {
        name : 'dojo',
        location : '/js/dojo'
    },
    {
        name : 'dojox',
        location : '/js/dojox'
    },
    {
        name : 'dijit',
        location : '/js/dijit'
    }
    ]
});
