function check(arr){
	function loop(ls, res){
		var rem = [];
		var res = res;
		
		if (ls == ""){
			return res;
		} else {
			var curr = ls[0];
		}

		for (var i = 1; i < ls.length; i++){			
			if (ls[i] != curr){
				rem.push(ls[i]);
			}
		}
		res.push(curr);
		return loop(rem, res);
	}

	return loop(arr, []);
}