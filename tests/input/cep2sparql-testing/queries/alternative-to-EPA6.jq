rule eventCounts(unit, unitDefault, extractor) {
     var unitValue = unitDefault;
     var count = 0;

     when {
     	?e: {"@type": "event", "time": ?time};
     } do {
	var newUnitValue = extractor(?time)
	if (newUnitValue == unitValue) {
	   count++;
	} else {
	  add {"@type": "eventCount", "time": ?time, unit: unitValue, "count": count};
	  count = 0;
	  unitValue = newUnitValue;
	}
    }
}

rule deleteOldEvents() {
     when {
     	?e1: {"@type": "event", "time": ?time1};
     	?e2: {"@type": "event", "time": ?time2};
        ?time1 < ?time2
     } do {
       remove ?e1;            
     }
}

input json from args[1];
new eventCounts("hour", -1, dateTime.hour);
new deleteOldEvents();
execute();
