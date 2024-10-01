var handleUploadFirmwareFileComplete = function(customEvent) {
    var zm = Tapestry.findZoneManagerForZone(customEvent.zone);
    zm.updateFromURL(customEvent.url);
};