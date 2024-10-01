require(['dojo/_base/declare', 'dojo/dom', 'dojo/dom-construct', 'dojo/_base/lang', 'dojo/data/ItemFileWriteStore',
        'dojo/data/ItemFileReadStore',
        'dojox/charting/DataChart', 'dojo/_base/xhr', 'dojo/_base/array', 'dijit/TitlePane', 'dijit/form/Button' ],
        function(declare, dom, domConstruct, lang, ItemFileWriteStore, ItemFileReadStore, DataChart, xhr, array, TitlePane, Button) {
            declare('FollowChart', null, {
                constructor : function(options) {
                    this.options = options;

                    this.values = new Array();

                    this.render();

                    this.updateValue();

                    this.interval = setInterval(lang.hitch(this, "updateValue"), 1000);

                    this.lastAddValueTime = 1;
                    this.evolution = 0;
                },

                updateValue : function() {
                    xhr.get({
                        url : this.options.url,
                        handleAs : 'json',
                        load : lang.hitch(this, "addValue")
                    });
                },

                addValue : function(xhrRet) {
                    var coef = this.retrieveCoef();
                    if (xhrRet.value == undefined) {
                        var value = null;
                    } else {
                        var value = parseInt(xhrRet.value * coef);
                    }
                    this.values.push(value);

                    if (this.values.length > 20) {
                        this.values.shift();
                    }

                    this.yMax = Math.max.apply(null, this.values);
                    this.yMin = Math.min.apply(null, this.values);

                    if (this.yMin == this.yMax) {
                        this.yMin = this.yMax - 50;
                        this.yMax = this.yMin + 100;
                    } else {
                        this.yMax += 10;
                        this.yMin -= 10;
                    }

                    this.yMajorTickStep = Math.floor((this.yMax - this.yMin) / 20);

                    this.calculateEvolution(xhrRet.time);

                    this.updateChart();
                },
                calculateEvolution: function(time) {
                    var diffTime = (time - this.lastAddValueTime) / 1000;
                    this.lastAddValueTime = time;

                    if (diffTime <= 2) {
                        var diffValue = this.values[this.values.length - 1] - this.values[this.values.length - 2];
                        this.evolution = Math.round(diffValue / diffTime);
                        this.updateEvolution();
                    }
                },

                updateEvolution : function() {
                    this.evolutionSpan.innerHTML = this.evolution;
                },

                updateChart : function() {
                    var yaxis = this.chart.getAxis("y");
                    yaxis.opt.min = this.yMin;
                    yaxis.opt.max = this.yMax;
                    yaxis.opt.majorTickStep = this.yMajorTickStep;
                    this.chart.removeSeries("Data");
                    this.chart.addSeries("Data", this.values, {
                        stroke : {
                            color : "blue"
                        },
                        fill : "lightblue"
                    });

                    this.chart.render();
                },

                retrieveCoef : function() {
                    var currentCoef = this.coefInput.value;
                    if (isNaN(currentCoef)) {
                        currentCoef = 1;
                        this.coefInput.value = 1;
                    }
                    return currentCoef;
                },

                destroy : function() {
                    clearInterval(this.interval);
                    this._destroy();
                    this.options.manager.deleteChart(this);
                },

                _destroy : function() {
                    if (this.chart !== undefined) {
                        this.chart.destroy();
                    }
                    if (this.stopButton !== undefined) {
                        this.stopButton.destroy();
                    }
                    if (this.titlePane !== undefined) {
                        this.titlePane.destroy();
                    }
                },

                createPane : function() {
                    this.titlePane = new TitlePane({
                        title : this.options.name,
                        content : '<div id="' + this.options.chartDivId + '"/>'
                    });

                    this.options.manager.container.insert({
                        bottom : this.titlePane.domNode
                    });
                },

                createEvolutionSpan : function() {
                    this.evolutionDiv = domConstruct.create("div", {
                        innerHTML : "Evolution per second:&nbsp;"
                    }, this.options.chartDivId, "after");
                    this.evolutionSpan = domConstruct.create("span", {
                        innerHTML : "0"
                    }, this.evolutionDiv, "last");
                },

                createCoefInput : function() {
                    this.coefDiv = domConstruct.create("div", {
                        style : "float: right;",
                        innerHTML : "Coefficient:&nbsp;"
                    }, this.evolutionDiv, "after");
                    this.coefInput = domConstruct.create("input", {
                        type : "text",
                        size : "5",
                        value : "1"
                    }, this.coefDiv, "last");
                },

                createStopButton : function() {
                    this.stopButton = new Button({
                        label : "Stop following",
                        onClick : lang.hitch(this, "destroy")
                    });

                    $(this.options.chartDivId).insert({
                        after : this.stopButton.domNode
                    });
                },

                createChart : function() {
                    this.chart = new DataChart(this.options.chartDivId, {
                        displayRange : 20,
                        scroll : true
                    });
                    this.chart.addAxis("x");

                    this.chart.addAxis("y", {vertical: true});
                },

                render : function() {
                    this._destroy();
                    this.createPane();
                    this.createChart();
                    this.createStopButton();
                    this.createEvolutionSpan();
                    this.createCoefInput();
                }
            });

            declare('FollowChartManager', null, {
                constructor : function(options) {
                    this.options = options;
                    this.container = $(this.options.followChartContainerDivId);
                    if (this.checkExists()) {
                        this.manager.container = $(this.options.followChartContainerDivId);
                        this.manager.reRenderCharts();
                        return this.manager;
                    }
                    retrieveManagersArray().push(this);
                    this.charts = new Array();
                    this.uniqueId = 0;
                },

                addChart : function(options) {
                    options.chartDivId = this.options.chartDivId + this.uniqueId++;
                    options.manager = this;
                    this.charts.push(new FollowChart(options));
                },

                deleteChart : function(chart) {
                    var index = this.charts.indexOf(chart);
                    var charts = this.charts.splice(index, 1);
                    delete charts[0];
                },

                destroyAll : function() {
                    array.forEach(this.charts, function(chart, i) {
                        chart.destroy();
                    });
                    delete this.charts;
                    this.charts = null;
                },

                checkExists : function() {
                    var managersArray = retrieveManagersArray();
                    this.manager = undefined;
                    array.forEach(managersArray, lang.hitch(this, "_checkExists"));
                    return this.manager !== undefined;
                },

                _checkExists : function(manager, i) {
                    if (manager.options.coreId == this.options.coreId) {
                        this.manager = manager;
                    }
                },

                reRenderCharts : function() {
                    array.forEach(this.charts, function(chart, i) {
                        chart.render();
                    });
                }
            });
        }
);

Tapestry.ajaxRequest = function(url, data) {
    require(["t5/core/dom"], function(dom) {
      dom.ajaxRequest(url, data);
    })
}

Tapestry.loadScriptsInReply = function (reply, callback) {
    require(["t5/core/pageinit"], function (pageinit) {
        pageinit.handlePartialPageRenderResponse({json: reply}, callback);
    });
}

Tapestry.findZoneManagerForZone = function (zoneElement) {
            var element = $(zoneElement);
            if (!element) {
                Tapestry.error(Tapestry.Messages.missingZone, {
                    id: zoneElement
                });
                return null;
            }
            var manager = $T(element).zoneManager;
            if (!manager) {
                Tapestry.error(Tapestry.Messages.noZoneManager, element);
                return null;
            }
            return manager;
        };


Tapestry.dojo = {
    findTabContainerManagerForTabContainer : function(tabContainerId) {
        tabContainerManager = $(tabContainerId).getStorage().tabContainerManager;
        if (Object.isUndefined(tabContainerManager)) {
            tabContainerManager = new Tapestry.dojo.TabContainerManager(tabContainerId);
            $(tabContainerId).getStorage().tabContainerManager = tabContainerManager;
        }
        return tabContainerManager;
    }
};

Tapestry.dojo.TabContainerManager = Class.create({
    initialize : function(tabContainerId, options) {
        this.tabContainerId = tabContainerId;
        require(['dijit/layout/TabContainer'], function(t){
            this.tabContainer = dijit.byId(tabContainerId);
            if (this.tabContainer == null || this.tabContainer.declaredClass != 'dijit.layout.TabContainer') {
                throw tabContainerId + ' is not a dijit.layout.TabContainer.'
            }
            if (!Object.isUndefined(options) && Object.isFunction(options.onChangeTab)) {
                this.tabContainer.watch("selectedChildWidget", options.onChangeTab);
            }
        });
    },

    loadTabFromURL : function(url, id, tabTitle, ajaxParams) {
        if (Object.isUndefined(ajaxParams)) {
            ajaxParams = {}
        }

        Tapestry.ajaxRequest(url, {
            parameters : ajaxParams,
            success : function(transport) {
                this.processReply(transport.json, id, tabTitle);
            }.bind(this)
        });
    },

    processReply : function(reply, id, tabTitle) {
        Tapestry.loadScriptsInReply(reply, function() {

            var tabContainer = dijit.byId("tabContainer");
            children = tabContainer.getChildren();
            var pane;
            for (i = 0; i < children.length; i++) {
                if (children[i].get("title") == tabTitle) {
                    pane = children[i];
                }
            }

            if (Object.isUndefined(pane)) {
                pane = new dijit.layout.ContentPane({
                    enyxId: id,
                    title : tabTitle,
                    content : reply.content,
                    closable : true,
                    parseOnLoad : false
                });
                tabContainer.addChild(pane);
            } else {
                pane.set('content', reply.content);
            }
            tabContainer.selectChild(pane);

        }.bind(this));
    }
});

require(['dojo', 'dojo/parser',
    'dojo/number',
    'dojo/data/ItemFileWriteStore',
    'dijit/form/Button',
    'dijit/form/ComboBox',
    'dijit/form/Select',
    'dijit/form/TextBox',
    'dojox/form/Uploader',
    'dijit/layout/BorderContainer',
    'dijit/layout/ContentPane',
    'dijit/layout/TabContainer',
    'dijit/tree/ForestStoreModel',
    'dijit/TitlePane',
    'dijit/Tooltip',
    'dijit/Tree',
    'dijit/form/Select'], function(){
})


Tapestry.dojo.Initializer = {};

Tapestry.Initializer.tabContainer = function(spec) {
    require(["dojo"], function(dojo) {
        dojo.ready(function() {
            storage = $(spec.elementId).getStorage();
            tabContainerManager = new Tapestry.dojo.TabContainerManager(spec.elementId, spec);
            storage.tabContainerManager = tabContainerManager;
        });
    });
}
