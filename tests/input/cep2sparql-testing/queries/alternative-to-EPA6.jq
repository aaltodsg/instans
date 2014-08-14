rule eventCounts(unit, extractor)
when {
     ?e: {"@type": "event", "time": ?time, *};
     ?c: {"@type": "counter", "count": ?count = -1, unit: ?unitValue = -1} = none
} do {
      remove ?e;
      ?newUnitValue = extractor(?time)
      if (?newUnitValue == ?unitValue) {
      	 ?c.count = ?count+1;
      } else {
      	add {"@type": "eventCount", "time": ?time, unit: ?unitValue, "count": ?count};
	remove ?c;
	add {"@type": counter, "count": 1, unit: ?newUnitValue }
    }
  }
}

input json from args[1];
eventCounts("hour", dateTime.hour);


