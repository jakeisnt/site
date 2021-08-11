function shuffleTitle(
  titleStrings = ["JAKEISNT"],
	repeatForever = false,
	numRepeats = 3,
	speed = 80,
	inputDelay = 100,
	extraChars = "★☆✚☯❤☺⚠❁❊✳✩◍░▊▐☰𝍖⌖⌬⍜⍚⎕⎖〪〫❀",
) {
	const availableChars = extraChars + Array.from(new Set(titleStrings.join('').split(''))).join('');

	let numCycles = 1;
	let numCorrect = 0;
	let currMsgIdx = 0;
	let timer = null;
	let canContinue = true;

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

	function setChar(k){
		r = Math.round(Math.random() * availableChars.length);
		sts[k] = availableChars.charAt(r);

		document.title = sts.join('');

		if (availableChars.charAt(r) == titleStrings[currMsgIdx].charAt(k)) {
			numCorrect++;
			return;
		}

		setTimeout(() => setChar(k), speed);
	}

	function init(){
		if (canContinue) {
			titleStrings[currMsgIdx].split('').forEach((char, charIndex) => {
				msgchar = char;
				setChar(charIndex);
			})

			canContinue = false;
			delay = 200;
		}

		if (numCorrect == titleStrings[currMsgIdx].length){
			if (currMsgIdx == titleStrings.length-1){
				if ((!repeatForever) && (numCycles == numRepeats)){
					clearTimeout(timer);
					return;
				}
				currMsgIdx = 0;
				numCycles++;
			} else {
				currMsgIdx++;
			}

			canContinue = true;
			numCorrect = 0;
			delay = inputDelay;
		}

		timer = setTimeout(init, delay);
	}

	arr = shuffle(fillRange(0, titleStrings[currMsgIdx].length));
	sts = new Array(titleStrings[currMsgIdx].length);

	titleStrings[currMsgIdx].split('').forEach((_, i, currMsg) => {
		sts[i] = currMsg[arr[i]];
	});

	init();
}

shuffleTitle();
