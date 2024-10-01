
var retrieveManagersArray = function() {
    var hardwareInspector = $('hardwareInspectorContainer');
    var storage = hardwareInspector.getStorage();
    var managersArray = storage.followChartManagers;
    if (managersArray === undefined) {
        managersArray = new Array();
        storage.followChartManagers = managersArray;
    }
    return managersArray;
};

var addChart = function(coreId, options) {
    var managersArray = retrieveManagersArray();
    for ( var iAddChart = 0; iAddChart < managersArray.length; iAddChart++) {
        if (coreId == managersArray[iAddChart].options.coreId) {
            managersArray[iAddChart].addChart(options);
            break;
        }
    }
};

var handleUploadXmlFileComplete = function(customEvent) {
    var zm = Tapestry.findZoneManagerForZone(customEvent.zone);
    zm.updateFromURL(customEvent.url);
};
