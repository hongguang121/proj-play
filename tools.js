//fun: string
function time (fun) {
	var start = new Date();
	eval(fun);
	console.log(new Date() - start);
}

function fib (n) {
	if (n < 2)
		return n;
	else return fib(n - 1) + fib(n - 2);
}

function remove(arr, id){
	var res = [];
	for (var i = 0; i < arr.length; i++){
		if (i != id){
			res.push(arr[i]);
		}
	}
	return res;
}

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

function loop(callback, time){  //callback: reference
	setInterval(function(){
		callback();  //value
		loop(callback, time);
	}, time);
}

//loop(function(){ function call ... }, time);

(function(){
	var fact = function(n){
		if (n == 0){
			return 1;
		} else {
			return n * fact(n - 1);
		}
	}
	return fact;
})()(6)

//异步加载图片
function loadImageAsync(url) {
  return new Promise(function(resolve, reject) {
    var image = new Image();

    image.onload = function() {
      resolve(image);
    };

    image.onerror = function() {
      reject(new Error('Could not load image at ' + url));
    };

    image.src = url;
  });
}

//Promise Ajax Request
var getJSON = function(url) {
  var promise = new Promise(function(resolve, reject){
    var client = new XMLHttpRequest();
    client.open("GET", url);
    client.onreadystatechange = handler;
    client.responseType = "json";
    client.setRequestHeader("Accept", "application/json");
    client.send();

    function handler() {
      if (this.readyState !== 4) {
        return;
      }
      if (this.status === 200) {
        resolve(this.response);
      } else {
        reject(new Error(this.statusText));
      }
    };
  });

  return promise;
};

getJSON("/posts.json").then(function(json) {
  console.log('Contents: ' + json);
}, function(error) {
  console.error('出错了', error);
});

try {
    throw new Error('test');
  } catch(e) {
    console.log(e);
}

function* gen(x) {
  var y = yield x + 2;
  return y;
}

//java curry
//Function<Integer, Function<Integer, Integer>> add = (x) -> (y) -> x + y;
//add.apply(1).apply(1) = 2;

//java的文法作用域和racket相同
List<Supplier<Integer>> sqList(){
  List<Supplier<Integer>> res = new ArrayList<>();
  Function<Integer, Integer> square = x -> x * x;
  for (int i = 1; i < 5; i++){
  	final int n = i;
  	Supplier<Integer> sp = () -> square.apply(n);
    res.add(sp);
  }
  return res;
}

List<Supplier<Integer>> res = sqList();
res.forEach(rf -> System.out.println(rf.get()));
for (int i = 0; i < res.size(); i++){
	println(res.get(i).get());
}

long fib(int n){
	return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

//js
var sqList = function() {
	var res = [];
	for (var i = 0; i < 5; i++){
		res.push(function() { return i * i; });
	}
	return res;
}

sqList.map( f => { return f(); });

//java map
Map<Integer, String> luckyNumbers = Map.of(1, "One", 2, "Two");
luckyNumbers.entrySet().iterator().next().getKey();
luckyNumbers.entrySet().iterator().next().getValue();

//没有就添加有就删除
var addRem = function(arr, id){
	var res = [];
	var flag = 0
	for (var i = 0; i < arr.length; i++){
		if (arr[i] == id){
			flag = 1;		
		} else {
			res.push(arr[i]);
		}
	}	

	if (flag == 0){
		res.push(id);
	}

	return res;
}

//UnaryOperator
UnaryOperator<String> uo = (String name) -> "Hi! I'm " + name;
Function<String, String> say = uo;
say.apply("Yaoer");

//higher-order
def apply(f: Int => String, v: Int) = f(v)

def intToStr(i: Int): String = i.toString

apply(intToStr, i)

//java thread
Function<Long, Long> fib =
	n -> {
		if (n < 2) return n;
		else return fib.apply(n - 1) + fib.apply(n - 2);
	};

Runnable r = () -> System.out.println(fib.apply(40L));

Thread t = new Thread(r);
t.start();

//java future ~ racket delay/thread
CompletableFuture<Long> cf = new CompletableFuture<>();
new Thread(() -> cf.complete(fib.apply(45L))).start();
cf.get(); //未计算出值时会阻塞，结果值会永久保存在cf对象中，故新的计算要初始化新的CompletableFuture对象

CompletableFuture cf = CompletableFuture.supplyAsync(() -> fib.apply(45L));
cf.get();

list.stream().map( x -> x * x).collect(Collectors.toList());

List<Long> lt = Arrays.asList(10L, 20L, 30L, 40L);
List<CompletableFuture<Long>> res = lt.stream()
	.map(e -> CompletableFuture.supplyAsync(() -> fib.apply(e)))
	.collect(Collectors.toList());

	res.stream()
       .map(CompletableFuture::join) //等待所有异步操作结束
       .collect(Collectors.toList());
