"use strict";

exports.mouse_event = function (evt) {
    return function() {
        console.log(evt);
    };
};
