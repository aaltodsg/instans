function addClass(elem, className) {
    elem.attr('class', function(index, classNames) {
	if (typeof classNames == 'undefined') {
	    classNames = '';
	}
	var newcls = classNames + ' ' + className; console.log('ClassNamesA = ' + newcls); return newcls;});
}

function removeClass(elem, className) {
    elem.attr('class', function(index, classNames) { var newcls = classNames.replace(className, ''); console.log('ClassNamesR = ' + newcls); return newcls;});
}

function findNode(name) {
    var node = $("#"+name);
    console.log('Finding node ' + name);
    console.log(node);
    return node
}

function findEdge(fromName, toName) {
    console.log('Finding edge ' + fromName + ' to ' + toName);
    var edge = $("#"+fromName+"_to_"+toName);
    console.log(edge);
    return edge;
}

function enterNode(nodeName) {
    console.log('entering node ' + nodeName);
    addClass(findNode(nodeName).find('ellipse'), 'active');
    addClass(findNode(nodeName).find('ellipse'), 'current');
};

function exitNode(nodeName) {
    console.log('exiting node ' + nodeName);
    removeClass(findNode(nodeName).find('ellipse'), 'current');
    removeClass(findNode(nodeName).find('ellipse'), 'active');
};

function callChild(nodeName, childNodeName) {
    var edge = findEdge(nodeName, childNodeName);
    addClass(edge.find('path'), 'visited');
    var node = findNode(nodeName);
    removeClass(node.find('ellipse'), 'current');
    enterNode(childNodeName);
};

function reteAdd(s, p, o, g) {
    $("#rete-operation-name").html('Add');
    $("#rete-operation-graph").html(g);
    $("#rete-operation-subject").html(s);
    $("#rete-operation-predicate").html(p);
    $("#rete-operation-object").html(o);
}

function reteRemove(s, p, o, g) {
    $("#rete-operation-name").html('Remove');
    $("#rete-operation-graph").html(g);
    $("#rete-operation-subject").html(s);
    $("#rete-operation-predicate").html(p);
    $("#rete-operation-object").html(o);
}

