title_strings = ["JAKEISNT"];

const repeat_forever = true;
const num_repeats = 1;
const speed = 80;
const inputDelay = 100;
const available_chars = "★☆✚☯❤☺⚠❁❊✳✩◍░▊▐☰𝍖⌖⌬⍜⍚⎕⎖〪〫❀" + Array.from(new Set(title_strings.join('').split(''))).join('');

let num_cycles = 1;
let count_affected = 0;
let currMsgIdx = 0;
let timer = null;
let can_continue = true;

// swap all of the elements in the provided array with one another at random
function shuffle(arr){
	for (i=0; i < arr.length; i++){
		let k = Math.round(Math.random() * (arr.length - i - 1)) + i;
		temp = arr[i];arr[i]=arr[k];arr[k]=temp;
	}
	return arr;
}

// fill an array with a sequence of characters starting at start
function fillRange(start, end) {
  return new Array(end - start + 1).fill().map((item, index) => start + index);
};

arr = shuffle(fillRange(0, title_strings[currMsgIdx].length));
sts = new Array(title_strings[currMsgIdx].length);

for (var i=0; i<title_strings[currMsgIdx].length; i++){
	sts[i] = title_strings[currMsgIdx].charAt(arr[i]);
}


function setChar(k){
	r = Math.round(Math.random() * available_chars.length);
	sts[k] = available_chars.charAt(r);

	document.title = sts.join('');

	if (available_chars.charAt(r) == title_strings[currMsgIdx].charAt(k)){
		stsmsg = "";
		for (var i=0; i<sts.length; i++) stsmsg += sts[i];
		document.title = stsmsg;
		count_affected++;
		return;
	}

	setTimeout(() => setChar(k), speed);
}

function init(){
	if (can_continue) {
		title_strings[currMsgIdx].split('').forEach((char, charIndex) => {
			msgchar = char;
			setChar(charIndex);
		})

		can_continue = false;
		delay = 200;
	}

	if (count_affected == title_strings[currMsgIdx].length){
		if (currMsgIdx == title_strings.length-1){
			if ((!repeat_forever) && (num_cycles == num_repeats)){
				clearTimeout(timer);
				return;
			}
			currMsgIdx = 0;
			num_cycles++;
		} else {
			currMsgIdx++;
		}

		arr = shuffle(fillRange(0, title_strings[currMsgIdx].length));
		sts = new Array(title_strings[currMsgIdx].length);

		title_strings[currMsgIdx].split('').forEach((_, i, currMsg) => {
			sts[i] = currMsg[arr[i]];
		});

		can_continue = true;
		count_affected = 0;
		delay = inputDelay;
	}

	timer = setTimeout(init, delay);
}

init();
