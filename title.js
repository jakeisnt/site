
function tb4_makeArray(n){
	this.length = n;
	return this.length;
}

site_name = "JAKEISNT";

// array of size 1?
tb4_messages = [site_name];

repeat_forever = false;
num_repeats = 1;

speed = 80;
inputDelay = 100;

var num_cycles = 1;

var available_chars = site_name + "★☆✚☯❤☺⚠❁❊✳✩◍░▊▐☰𝍖⌖⌬⍜⍚⎕⎖〪〫❀";
var count_affected = 0;
var tb4_stsmsg = "";
var tb4_currMsg = 0;
var tb4_timer = null;
var can_continue=true;

// determine whether a string contains a character
function string_contains(str, ch) {
	return str.search(ch) == -1;
}

// addss a new character to the chars to choose from if it doesn't already exist
function tb4_addChar(ch){
	if (string_contains(available_chars, ch)){
		available_chars = available_chars+ch;
	}
}

// for each char of each possible message, add it to the available chars
tb4_messages.forEach(function(message) {
	message.split('').forEach(tb4_addChar)
});

function tb4_shuffle(arr){
	var k;
	for (i=0; i<arr.length; i++){
		k = Math.round(Math.random() * (arr.length - i - 1)) + i;
		temp = arr[i];arr[i]=arr[k];arr[k]=temp;
	}
	return arr;
}

tb4_arr = new tb4_makeArray(tb4_messages[tb4_currMsg].length);
tb4_sts = new tb4_makeArray(tb4_messages[tb4_currMsg].length);

for (var i=0; i<tb4_messages[tb4_currMsg].length; i++) tb4_arr[i] = i;

tb4_arr = tb4_shuffle(tb4_arr);

for (var i=0; i<tb4_messages[tb4_currMsg].length; i++){
	tb4_sts[i] = tb4_messages[tb4_currMsg].charAt(tb4_arr[i]);
}

function tb4_setChar(k){
	r = Math.round(Math.random() * available_chars.length);
	tb4_sts[k] = available_chars.charAt(r);
	tb4_stsmsg = "";
	for (var i=0; i<tb4_sts.length; i++)
	tb4_stsmsg += tb4_sts[i];
	document.title = tb4_stsmsg;
	if (available_chars.charAt(r) == tb4_messages[tb4_currMsg].charAt(k)){
		tb4_stsmsg = "";
		for (var i=0; i<tb4_sts.length; i++) tb4_stsmsg += tb4_sts[i];
		document.title = tb4_stsmsg;
		count_affected++;
		return;
	}
	setTimeout("tb4_setChar("+k+")", speed);
}
function init(){
	if (can_continue){
		tb4_messages[tb4_currMsg].split('').forEach(function(char, charIndex) {
			tb4_msgchar = char;
			tb4_setChar(charIndex);
		})

		can_continue = false;
		tb4_delay=200;
	}

	if (count_affected == tb4_messages[tb4_currMsg].length){
		if (tb4_currMsg == tb4_messages.length-1){
			if ((!repeat_forever) && (num_cycles == num_repeats)){
				clearTimeout(tb4_timer);
				return;
			}
			tb4_currMsg=0;
			num_cycles++;
		}
		else {
			tb4_currMsg++;
		}
		tb4_arr = new tb4_makeArray(tb4_messages[tb4_currMsg].length);
		tb4_sts = new tb4_makeArray(tb4_messages[tb4_currMsg].length);
		for (var i=0; i<tb4_messages[tb4_currMsg].length; i++){
			tb4_arr[i] = i;
			can_continue = true;
			count_affected = 0;
			tb4_delay = inputDelay;
		}

		tb4_arr = tb4_shuffle(tb4_arr);
		for (var i=0; i<tb4_messages[tb4_currMsg].length; i++){
			tb4_sts[i] = tb4_messages[tb4_currMsg].charAt(tb4_arr[i]);
		}
	}

	tb4_timer = setTimeout("init()", tb4_delay);
}

init();
