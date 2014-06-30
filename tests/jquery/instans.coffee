findNode = (name) -> $('#'+name)

findEdge = (fromName, toName) -> $('#'+fromName+'_to_'+toName)

addClass = (elem, cl) -> 
	elem.attr('class', (index, classNames) ->
		if (typeof classNames == 'undefined') then classNames = ''
		newcls = classNames + ' ' + className
		console.log(elem)
		console.log('ClassNames after add = ' + newcls)
		newcls)

removeClass = (elem, cl) -> 
	elem.attr('class', (index, classNames) ->
		newcls = classNames.replace(className, '')
		console.log(elem)
		console.log('ClassNames after remove = ' + newcls)
		newcls)

operationPrettyName = (opn) -> opn

nodePrettyName = (nn) -> nn

tokenPretty = (token) ->
	result = ""
	for [variable, value] in token
		result = result + "<span class=\"sparql-binding\"><span class=\"sparql-var\">#{variable}</span> &rarr; <span class=\"sparql-value\">#{value}</span></span>"
	result

class ReteOpStack
	constructor: (@element) -> @element.html("");

	lastEntry: () ->
		$(@element).find('div[class="stack-entry"]').last()


	enterOp: (operation, node, token) ->
		console.log("enterOp '#{operation}', '#{node}', '#{token}'")
		$(@element).append('<div class="stack-entry"><span class="operation"></span><span class="node"></span><span class="token"></span></div>')
		entry = @lastEntry()
		oe = entry.find('span[class="operation"]')
		ne = entry.find('span[class="node"]')
		te = entry.find('span[class="token"]')
		oe.attr('operation', operation)
		ne.attr('node', node)
		te.attr('token', token)
		oe.html(operationPrettyName(operation))
		ne.html(nodePrettyName(node))
		te.html(tokenPretty(token))
		addClass(findNode(node), 'current')

	exitOp: (operation, node, token) ->
		@lastEntry.remove()
		removeClass(findNode(node), 'current')

$ -> 
	opStack = new ReteOpStack($('#rete-node-stack'))
	console.log(opStack)
	opStack.enterOp('add token', 'triple_pattern_node2', [[0, 'ep:eve0'], [1, '"2014-01-07T09:18:21"^^xsd:dateTime']])
