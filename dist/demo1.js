"use strict";

//const unlistener = PS.Main.initEventsOrig();
const unlistener = PS.Main.initEvents();
//const unlistener = PS.Main.fn2(1234);

console.log(unlistener);

button.onclick = () => {
    unlistener(3)();
};
