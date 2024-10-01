var treeGetIconClass = function(item, opened) {
    if (item != this.model.root) {
        return item.type + 'Icon';
    }
};

var treeOnclick = function(url, item) {
    var tabContainerManager = Tapestry.dojo.findTabContainerManagerForTabContainer('tabContainer');
    tabContainerManager.loadTabFromURL(url + '?uri=' + encodeURIComponent(item.uri), item.uri, item.title);
};

var onChangeTab = function(url, newTab) {
    if (newTab.enyxId !== undefined) {
        Tapestry.ajaxRequest(url + '?uri=' + encodeURIComponent(newTab.enyxId));
    } else {
        Tapestry.findZoneManagerForZone("homeTab").updateFromURL(url + '?uri=homeTab');
    }
};

var calculateHeight = function() {
    var container = $('hardwareInspectorContainer');
    var newHeight = (window.innerHeight - 52) + 'px';
    container.setStyle({
        height : newHeight
    });
};

var maintainSessionOpen = function(url) {
    setInterval("Tapestry.ajaxRequest('" + url + "')", 15 * 60 * 1000);
};

var scrollLogToBottom = function() {
    require(['dojo', function(dojo) {
    dojo.ready(function() {
        log = $('log');
        log.scrollTop = log.scrollHeight;
    });
    }]);
};