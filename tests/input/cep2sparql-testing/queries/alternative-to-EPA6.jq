add {type: counter, count: -1, hour: -1}

input json from args[1] to rete;

rete match {
    ?e: {type: event, time: ?time, *};
    ?c: {type: counter, count: ?count, hour: ?ch}
} do {
    remove ?e;
    if (hour(?time) == ?time) {
	?c.count = ?count+1;
    } else {
	add {type: eventCount, time: ?time, hour: ?ch, count: ?count};
	remove ?c;
	add {type: counter, count: 0, hour: ?hour};
    }
}
